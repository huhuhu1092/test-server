#include "SE_Ase.h"
#include "SE_Log.h"
#include "SE_GeometryData.h"
#include "SE_ResourceManager.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
#include "SE_ResFileHeader.h"
#include "SE_File.h"
#include "SE_Application.h"
#include "SE_CommonNode.h"
#include "SE_Geometry.h"
#include "SE_BoundingVolume.h"
#include "SE_MeshSimObject.h"
#include "SE_IO.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
static int getFileLen(FILE* fp)
{
	int		pos;
	int		end;

	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);

	return end;

}
struct ase_t
{
    char* buffer;
    char* curpos;
    int len;
};

static ase_t ase; 
static char s_token[1024];
static char meshFileName[256];
static bool _verbose = false;
static const int MAGIC = 0xCFCFCFCF;
static const int VERSION = 0x01;
static const int COORDINATE = 0x00;
static const int ENDIAN = 0x00; /** 0: little endian, 1: big endian*/


static const short MATERIAL_ID = 0x0002;
static const short GEOMOBJECT_ID = 0x0003;
static const short CAMERA_ID = 0x0004;
static const short SUB_MATERIAL_ID = 0x0005;
static const short MESH_ID = 0x0006;


ASE_Loader::ASE_Loader() : mCurrGeomObject(NULL), mCurrMtl(NULL),mCurrSubMtl(NULL),mCurrMesh(NULL), mInSubDiffuse(false)
{
    mSceneObject = new ASE_SceneObject;
}
ASE_Loader::~ASE_Loader()
{
    delete mSceneObject;
}
/*
void ASE_Loader::Load()
{
    ASE_Load(meshFileName, _verbose);

}
*/
struct _MaterialData
{
    int subMaterialNum;
    ASE_MaterialData md;
    SE_MaterialDataID mid;
    SE_ImageDataID tid;
    _MaterialData()
    {
        subMaterialNum = 0;
    }
};
struct _GeomTexCoordData
{
    SE_GeometryDataID geomID;
    SE_TextureCoordDataID texCoordID;
};
static void writeHeader(SE_BufferOutput& output, int dataLen)
{
    output.writeInt(SE_MAGIC);
    output.writeInt(SE_VERSION);
    output.writeInt(SE_ENDIAN);
    output.writeInt(dataLen);
}
class _WriteSceneTravel : public SE_SpatialTravel
{
public:
	_WriteSceneTravel(SE_BufferOutput& out) : mOut(out)
	{}
	int visit(SE_Spatial* spatial)
	{
		spatial->write(mOut);
		return 0;
	}
    int visit(SE_SimObject* simObject)
	{
		return 0;
	}
public:
	SE_BufferOutput& mOut;
};
static const int SLEEP_COUNT = 50;
void ASE_Loader::Write(SE_BufferOutput& output, SE_BufferOutput& outScene, const char* shaderPath)
{
    int materialNum = mSceneObject->mMats.size();
	int numWhichHasSubmaterial = 0;
	int materialRealNum = materialNum;
    int i;
	for(i = 0 ; i < materialNum ; i++)
	{
		ASE_Material* srcm = &mSceneObject->mMats[i];
		materialRealNum += srcm->numsubmaterials;
		if(srcm->numsubmaterials > 0)
		{
			numWhichHasSubmaterial++;
		}
	}
    std::vector<_MaterialData> materialVector(materialRealNum);
    std::vector<int> indexWhichHasSubmaterial(numWhichHasSubmaterial);
	int l = 0;
	int mi = 0;
    for(i = 0 ; i < materialNum ; i++)
    {
        ASE_Material* srcm = &mSceneObject->mMats[i];
        _MaterialData md;
        md.subMaterialNum = srcm->numsubmaterials;
        md.md = srcm->materialData;
        materialVector[mi++] = md;
        if(srcm->numsubmaterials > 0)
        {
            indexWhichHasSubmaterial[l++] = i;
        }
    }
    std::vector<int>::iterator it;
    for(it = indexWhichHasSubmaterial.begin() ; it != indexWhichHasSubmaterial.end() ; it++)
    {
        int index = *it;
        ASE_Material* m = &mSceneObject->mMats[index];
        for(int j = 0 ; j < m->numsubmaterials ; j++)
        {
            _MaterialData md;
            md.subMaterialNum = 0;
            md.md = m->submaterials[j];
            materialVector[mi++] = md;

        }
    }
    std::vector<_MaterialData>::iterator itMaterial;
    output.writeShort(SE_MATERIALDATA_ID);
	output.writeInt(materialVector.size());
	int mmm = materialVector.size();
    //for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    for(i = 0 ; i < materialVector.size() ; i++)
    {
        SE_MaterialDataID mid = SE_Application::getInstance()->createCommonID();
        mid.print();
		//SE_Util::sleep(SLEEP_COUNT);
        materialVector[i].mid = mid;
        mid.write(output);
        output.writeVector3f(materialVector[i].md.ambient);
        output.writeVector3f(materialVector[i].md.diffuse);
        output.writeVector3f(materialVector[i].md.specular);
        output.writeVector3f(SE_Vector3f(0, 0, 0));
    }
    /////////////////////////////write texture data ///////////////
    output.writeShort(SE_IMAGEDATA_ID);
    int imageDataNum = 0;
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        std::string texStr(itMaterial->md.texName);
        if(texStr != "")
        {
            imageDataNum++;
        }
    }
    output.writeInt(imageDataNum);
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        std::string texStr(itMaterial->md.texName);
        if(texStr != "")
        {
            size_t pos = texStr.find('.');
            std::string name = texStr.substr(0, pos);
			std::string filename = name + ".raw";
			SE_ImageDataID tid = name.c_str();//texStr.c_str();
            itMaterial->tid = tid;
            tid.write(output);
            output.writeInt(0); // image data type
            output.writeString(filename.c_str());
        }
    }
    /////////////////////////////write geom data /////////////////////////////////////////////
    output.writeShort(SE_GEOMETRYDATA_ID);
    int geomDataNum = mSceneObject->mGeomObjects.size();
    output.writeInt(geomDataNum);
    std::vector<_GeomTexCoordData> geomTexCoordData(geomDataNum);
    std::list<ASE_GeometryObject*>::iterator itGeomObj;
    int n = 0;
    SE_Matrix4f modelToWorldM, worldToModelM;
    SE_Matrix3f rotateM;
	SE_Quat rotateQ;
    SE_Vector3f rotateAxis, scale, translate;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
        itGeomObj != mSceneObject->mGeomObjects.end();
        itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_GeometryDataID gid = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
		rotateQ.set(go->rotateAngle, rotateAxis);
		rotateM = rotateQ.toMatrix3f();//.setRotateFromAxis(go->rotateAngle, rotateAxis);
        modelToWorldM.set(rotateM, scale, translate);
        worldToModelM = modelToWorldM.inverse();
        geomTexCoordData[n++].geomID = gid;
        gid.write(output);
        output.writeInt(mesh->numVertexes);
        output.writeInt(mesh->numFaces);
        output.writeInt(0);
        int i;
        for(i = 0 ; i < mesh->numVertexes ; i++)
        {
            SE_Vector4f p(mesh->vertexes[i].x, mesh->vertexes[i].y, mesh->vertexes[i].z, 1.0f);
            p = worldToModelM.map(p);
            output.writeFloat(p.x);
            output.writeFloat(p.y);
            output.writeFloat(p.z);
        }
        for(i = 0 ; i < mesh->numFaces ; i++)
        {
            output.writeInt(mesh->faces[i].vi[0]);
            output.writeInt(mesh->faces[i].vi[1]);
            output.writeInt(mesh->faces[i].vi[2]);
        }
    }

    ////////////////////////write texture coordinate///////////////////////////////////////////////
    output.writeShort(SE_TEXCOORDDATA_ID);
    output.writeInt(geomDataNum);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_TextureCoordDataID tcid = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        tcid.write(output);
        geomTexCoordData[n++].texCoordID = tcid;
        output.writeInt(mesh->numTVertexes);
		if(mesh->numTVertexes > 0)
            output.writeInt(mesh->numFaces);
		else
			output.writeInt(0);
        int i;
		if(mesh->numTVertexes > 0)
		{
			for(i = 0 ; i < mesh->numTVertexes ; i++)
			{
				output.writeFloat(mesh->tvertexes[i].s);
				output.writeFloat(mesh->tvertexes[i].t);
			}
			for(i = 0 ; i < mesh->numFaces ; i++)
			{
				output.writeInt(mesh->tfaces[i].vi[0]);
				output.writeInt(mesh->tfaces[i].vi[1]);
				output.writeInt(mesh->tfaces[i].vi[2]);
			}
		}
    }
///////////////////// write shader program ////
    output.writeShort(SE_SHADERPROGRAMDATA_ID);
    int spNum = 1;
    output.writeInt(spNum);// shader program num;
    std::vector<SE_ProgramDataID> programDataVector(spNum);
    for(i = 0 ; i < spNum ; i++)
    {
        SE_ProgramDataID proID = "main_vertex_shader";
        programDataVector[i] = proID;
        //SE_Util::sleep(SLEEP_COUNT);
        proID.write(output);
        std::string str(shaderPath);
        std::string vertexShaderPath = str + SE_SEP + "main_vertex_shader.glsl";
        std::string fragmentShaderPath = str + SE_SEP + "main_fragment_shader.glsl";
        char* vertexShader = NULL;
        int vertexShaderLen = 0;
        char* fragmentShader = NULL;
        int fragmentShaderLen = 0;
        SE_IO::readFileAll(vertexShaderPath.c_str(), vertexShader, vertexShaderLen);
        SE_IO::readFileAll(fragmentShaderPath.c_str(), fragmentShader, fragmentShaderLen);
        output.writeInt(vertexShaderLen);
        output.writeInt(fragmentShaderLen);
        output.writeBytes(vertexShader, vertexShaderLen);
        output.writeBytes(fragmentShader, fragmentShaderLen);
        delete[] vertexShader;
        delete[] fragmentShader;
    }
///////////////////// write mesh //////////////// 
    std::vector<SE_MeshID> meshIDVector(geomDataNum);
    output.writeShort(SE_MESHDATA_ID);
    output.writeInt(geomDataNum);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        meshID.write(output);
        meshIDVector[n] = meshID;
        SE_GeometryDataID geomID = geomTexCoordData[n].geomID;
        SE_TextureCoordDataID texCoordID = geomTexCoordData[n].texCoordID;
        n++;
        geomID.write(output);
        output.writeFloat(go->wireframeColor[0]);
        output.writeFloat(go->wireframeColor[1]);
        output.writeFloat(go->wireframeColor[2]);
        int texNum = 0;
        int materialref = go->materialref;
        int startpos = 0;
        int subMaterialStartPos = 0;
        _MaterialData mdData;
        if(materialref == -1)
        {
            output.writeInt(texNum);
            goto WRIET_SURFACE;
        }
        mdData = materialVector[materialref];
        if(mdData.subMaterialNum > 0)
        {
            int j;
            for(j = 0 ; j < (materialref - 1) ; j++)
            {
                _MaterialData d = materialVector[j];
                startpos += d.subMaterialNum;
            }
            int k = startpos;
            for(int j = 0 ; j < mdData.subMaterialNum ; j++)
            {
                _MaterialData subMaterialData = materialVector[materialNum + k];
                k++;
                std::string texStr(subMaterialData.md.texName);
                if(texStr != "")
                {
                    texNum++;
                }
            }
        }
        else
        {
            std::string texStr(mdData.md.texName);
            if(texStr != "")
            {
                texNum = 1;
            }
        }
        output.writeInt(texNum);
        for(i = 0 ; i < texNum ; i++)
        {
            if(mdData.subMaterialNum > 0)
            {
                int j;
                for(j = 0 ; j < (materialref - 1) ; j++)
                {
                    _MaterialData d = materialVector[j];
                    subMaterialStartPos += d.subMaterialNum;
                }
                for(int j = 0 ; j < mdData.subMaterialNum ; j++)
                {
                    _MaterialData subMaterialData = materialVector[materialNum + subMaterialStartPos];
                    subMaterialStartPos++;
                    std::string texStr(subMaterialData.md.texName);
                    if(texStr != "")
                    {
                        output.writeInt(1);//current we just has one texture unit;
                        output.writeInt(0);//texture unit type is TEXTURE0
                        texCoordID.write(output);
                        output.writeInt(1);//image num use in the texture unit. current it is not mipmap. so the num is 1
                        subMaterialData.tid.write(output);

                    }
                }
            }
            else
            {
                std::string texStr(mdData.md.texName);
                if(texStr != "")
                {
                    output.writeInt(1);//current we just has one texture unit;
                    output.writeInt(0);//texture unit type is TEXTURE0
                    texCoordID.write(output);
                    output.writeInt(1);//image num use in the texture unit. current it is not mipmap. so the num is 1
                    mdData.tid.write(output);
                }
            }
            
        }
        ///write surface
WRIET_SURFACE:
        if(mesh->numFaceGroup > 0)
        {
            SE_ASSERT(mesh->numFaceGroup == mesh->faceGroup.size());
            output.writeInt(mesh->numFaceGroup);
            std::vector<std::list<int> >::iterator itFaceGroup;
            int indexM = startpos;
            int texIndex = 0;
            for(itFaceGroup = mesh->faceGroup.begin() ; itFaceGroup != mesh->faceGroup.end(); itFaceGroup++)
            {
                _MaterialData md = materialVector[materialNum + indexM];
                std::string texStr(md.md.texName);
                md.mid.write(output);
                output.writeInt(itFaceGroup->size());
                std::list<int>::iterator itFace;
                for(itFace = itFaceGroup->begin() ; itFace != itFaceGroup->end() ; 
					itFace++)
                {
                    output.writeInt(*itFace);
                }
                programDataVector[0].write(output);
                if(texStr != "")
                {
                    output.writeInt(texIndex);
                }
                else
                {
                    output.writeInt(-1);
                }
                indexM++;
                texIndex++;
            }
        } 
        else
        {
            output.writeInt(1); //just has one surface
            std::string texStr(mdData.md.texName);
            mdData.mid.write(output);
            output.writeInt(mesh->numFaces); // facets num;
            for(int f = 0 ; f < mesh->numFaces ; f++)
                output.writeInt(f);
            programDataVector[0].write(output);
            if(texStr != "")
            {
                output.writeInt(0); // the texture index is 0;
            }
            else
            {
                output.writeInt(-1);
            }
        }
    }
    /////// create scene //////////
    SE_SpatialID spatialID = SE_Application::getInstance()->createCommonID();
    //SE_Util::sleep(SLEEP_COUNT);
    SE_CommonNode* rootNode = new SE_CommonNode(spatialID, NULL);
    rootNode->setBVType(SE_BoundingVolume::AABB);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = meshIDVector[n++];
        SE_SpatialID childID = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        SE_Geometry* child = new SE_Geometry(childID, rootNode);
        rootNode->addChild(child);
        SE_Vector3f translate, scale, rotateAxis;
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        child->setLocalTranslate(translate);
		//child->setLocalTranslate(SE_Vector3f(0, 0, 0));
        child->setLocalScale(scale);
        //child->setLocalScale(SE_Vector3f(1.0, 1.0, 1.0));
        SE_Quat q;
        q.set(go->rotateAngle, rotateAxis);
        child->setLocalRotate(q);
		//q.set(0, SE_Vector3f(0, 0, 0));
        child->setBVType(SE_BoundingVolume::AABB);
        SE_MeshSimObject* meshObj = new SE_MeshSimObject(meshID);
		meshObj->setName(go->name);
        child->attachSimObject(meshObj);
    }
    SE_SceneID sceneID = SE_Application::getInstance()->createCommonID();
    //SE_Util::sleep(SLEEP_COUNT);
    sceneID.write(outScene);
	_WriteSceneTravel wst(outScene);
	rootNode->travel(&wst, true);
    LOGI("write end\n");
}
void ASE_Loader::Write(const char* dataPath, const char* outFileName)
{
    SE_BufferOutput outBase, outScene;
    SE_BufferOutput outBaseHeader, outSceneHeader;
    Write(outBase, outScene, dataPath);
    writeHeader(outBaseHeader, outBase.getDataLen());
    writeHeader(outSceneHeader, outScene.getDataLen());
    std::string outBaseFileName(outFileName);
    outBaseFileName = outBaseFileName + "_basedata.cbf";
	SE_File fbase(outBaseFileName.c_str(), SE_File::WRITE);
    fbase.write(outBaseHeader);
    fbase.write(outBase);
    std::string outSceneFileName(outFileName);
    outSceneFileName = outSceneFileName + "_scene.cbf";
	SE_File fscene(outSceneFileName.c_str(), SE_File::WRITE);
    fscene.write(outSceneHeader);
    fscene.write(outScene);
}
/*
** ASE_Load
*/
void ASE_Loader::Load( const char *filename, bool verbose)
{
	FILE *fp = fopen( filename, "rb" );
        
	if ( !fp )
		LOGE( ("File not found '%s'", filename) );

	memset( &ase, 0, sizeof( ase ) );
	ase.len = getFileLen( fp );

	ase.curpos = ase.buffer = new char[ase.len];

	LOGI( "Processing '%s'\n", filename );

	if ( fread( ase.buffer, ase.len, 1, fp ) != 1 )
	{
		fclose( fp );
		LOGE( "fread() != -1 for '%s'", filename );
	}

	fclose( fp );

	ASE_Process();
}




int ASE_Loader::CharIsTokenDelimiter( int ch )
{
	if ( ch <= 32 )
		return 1;
	return 0;
}

int ASE_Loader::ASE_GetToken( bool restOfLine )
{
	int i = 0;

	if ( ase.buffer == 0 )
		return 0;

	if ( ( ase.curpos - ase.buffer ) == ase.len )
		return 0;

	// skip over crap
	while ( ( ( ase.curpos - ase.buffer ) < ase.len ) &&
		    ( *ase.curpos <= 32 ) )
	{
		ase.curpos++;
	}

	while ( ( ase.curpos - ase.buffer ) < ase.len )
	{
		s_token[i] = *ase.curpos;

		ase.curpos++;
		i++;

		if ( ( CharIsTokenDelimiter( s_token[i-1] ) && !restOfLine ) ||
			 ( ( s_token[i-1] == '\n' ) || ( s_token[i-1] == '\r' ) ) )
		{
			s_token[i-1] = 0;
			break;
		}
	}

	s_token[i] = 0;

	return 1;
}

void ASE_Loader::ASE_ParseBracedBlock( ParserFun parser )
{
	int indent = 0;

	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "{" ) )
		{
			indent++;
		}
		else if ( !strcmp( s_token, "}" ) )
		{
			--indent;
			if ( indent == 0 )
				break;
			else if ( indent < 0 )
				LOGE( "Unexpected '}'" );
		}
		else
		{
			if ( parser )
				(this->*parser)( s_token );
		}
	}
}

void ASE_Loader::ASE_SkipEnclosingBraces(  )
{
	int indent = 0;

	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "{" ) )
		{
			indent++;
		}
		else if ( !strcmp( s_token, "}" ) )
		{
			indent--;
			if ( indent == 0 )
				break;
			else if ( indent < 0 )
				LOGE( "Unexpected '}'" );
		}
	}
}

void ASE_Loader::ASE_SkipRestOfLine(  )
{
	ASE_GetToken( true );
}

void ASE_Loader::ASE_KeyMAP_DIFFUSE( const char *token )
{
    char buffer[1024], buff1[1024], buff2[1024];
    char *buf1, *buf2;
    int i = 0, count;

    if ( !strcmp( token, "*BITMAP" ) )
    {
		ASE_GetToken( false );

		strcpy( buffer, s_token + 1 );
		if ( strchr( buffer, '"' ) )
				*strchr( buffer, '"' ) = 0;
		int len = strlen(buffer);
		buf1 = buffer + len - 1;
		for(i = len - 1 ; i >=0 ; i--)
		{
			if(buf1 && (*buf1) != '\\')
			{
					buf1--;    
			}    
			else
			{
					break;
			}
		}
		strncpy(buff1, buf1 + 1, 1024);
		if(mInSubDiffuse)
		{
            strncpy(mCurrSubMtl->texName, buff1, 256);
			LOGI("sub material texname : %s\n", mCurrSubMtl->texName);
		}
		else
		{
		    strncpy(mCurrMtl->materialData.texName, buff1, 256);
			LOGI("material texname : %s\n", mCurrMtl->materialData.texName);
		}
    }
}
void ASE_Loader::ASE_KeyMAP_SUBMATERIAL(const char* token)
{
	if ( !strcmp( token, "*MAP_DIFFUSE" ) )
	{
		mInSubDiffuse = true;
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_DIFFUSE );
		mInSubDiffuse = false;
	}
	else if(!strcmp( token, "*MATERIAL_AMBIENT"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    
	    mCurrSubMtl->ambient[0] = r;
        mCurrSubMtl->ambient[1] = g;
        mCurrSubMtl->ambient[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_DIFFUSE"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrSubMtl->diffuse[0] = r;
        mCurrSubMtl->diffuse[1] = g;
        mCurrSubMtl->diffuse[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_SPECULAR"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrSubMtl->specular[0] = r;
        mCurrSubMtl->specular[1] = g;
        mCurrSubMtl->specular[2] = b;
	}
    //ASE_KeyMATERIAL(token);
}
void ASE_Loader::ASE_KeyMATERIAL( const char *token )
{
 //   ASE_Material_t currMtl = mMtlList[mCurrMtl];
	if ( !strcmp( token, "*MAP_DIFFUSE" ) )
	{
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_DIFFUSE );
	}
	else if(!strcmp( token, "*MATERIAL_AMBIENT"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    
	    mCurrMtl->materialData.ambient[0] = r;
        mCurrMtl->materialData.ambient[1] = g;
        mCurrMtl->materialData.ambient[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_DIFFUSE"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrMtl->materialData.diffuse[0] = r;
        mCurrMtl->materialData.diffuse[1] = g;
        mCurrMtl->materialData.diffuse[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_SPECULAR"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrMtl->materialData.specular[0] = r;
        mCurrMtl->materialData.specular[1] = g;
        mCurrMtl->materialData.specular[2] = b;
	}
	else if(!strcmp( token, "*NUMSUBMTLS"))
	{
	    ASE_GetToken(false);
	    LOGI("...sub mtl num : %s\n", s_token);
	    int numsubmtl = atoi(s_token);
            //ASE_Material_t currMtl = mMtlList[mCurrMtl];
	    mCurrMtl->numsubmaterials = numsubmtl;
	    mCurrMtl->submaterials = new ASE_MaterialData[numsubmtl];
	}
	else if(!strcmp(token , "*SUBMATERIAL"))
	{
	    ASE_GetToken(false);
        int nCurrSubMtl = atoi(s_token);
	    mCurrSubMtl = &mCurrMtl->submaterials[nCurrSubMtl];
        ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_SUBMATERIAL );
	}
}

void ASE_Loader::ASE_KeyMATERIAL_LIST( const char *token )
{
	if ( !strcmp( token, "*MATERIAL_COUNT" ) )
	{
		ASE_GetToken( false );
		LOGI( "..num materials: %s\n", s_token  );
		mSceneObject->mMats.resize(atoi(s_token));
	}
	else if ( !strcmp( token, "*MATERIAL" ) )
	{
		ASE_GetToken(false);
        LOGI(  "..material %s \n",  s_token  );
        int nCurrMtl = atoi(s_token);
		mCurrMtl = &mSceneObject->mMats[nCurrMtl];
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMATERIAL );
	}
}
void ASE_Loader::ASE_KeyMESH_VERTEX_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_VERTEX" ) )
	{
		float x, y, z;
		ASE_GetToken( false );		// skip number
        int index = atoi(s_token);
		ASE_GetToken( false );
        x = atof(s_token);
		
		ASE_GetToken( false );
		y = atof(s_token);

		ASE_GetToken( false );
		z = atof(s_token);

		pMesh->vertexes[index].x = x ;
		pMesh->vertexes[index].y = y ;
		pMesh->vertexes[index].z = z ;


	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_VERTEX_LIST", token );
	}
}

void ASE_Loader::ASE_KeyMESH_FACE_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_FACE" ) )
	{
		ASE_GetToken( false );	// skip face number
        int index = atoi(s_token);
		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// first vertex
		pMesh->faces[index].vi[0] = atoi( s_token );

		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// second vertex
		pMesh->faces[index].vi[1] = atoi( s_token );

		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// third vertex
		pMesh->faces[index].vi[2] = atoi( s_token );

		ASE_GetToken( true );

        char* p;
		if ( ( p = strstr( s_token, "*MESH_MTLID" ) ) != 0 )
		{
			p += strlen( "*MESH_MTLID" ) + 1;
			pMesh->faces[index].materialID = atoi( p );
		}
		else
		{
			LOGE( "No *MESH_MTLID found for face!" );
		}
	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_FACE_LIST", token );
	}
}

void ASE_Loader::ASE_KeyTFACE_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_TFACE" ) )
	{
		int a, b, c;

		ASE_GetToken( false );
        int index = atoi(s_token);
		ASE_GetToken( false );
		a = atoi( s_token );
		ASE_GetToken( false );
		b = atoi( s_token );
		ASE_GetToken( false );
		c = atoi( s_token );

		LOGI(  ".....tface: %d\n", index );
		pMesh->tfaces[index].vi[0] = a;
		pMesh->tfaces[index].vi[1] = b;
		pMesh->tfaces[index].vi[2] = c;

	}
	else
	{
		LOGE( "Unknown token '%s' in MESH_TFACE", token );
	}
}

void ASE_Loader::ASE_KeyMESH_TVERTLIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_TVERT" ) )
	{
		char u[80], v[80], w[80];

		ASE_GetToken( false );
        int index = atoi(s_token);
		ASE_GetToken( false );
		strcpy( u, s_token );

		ASE_GetToken( false );
		strcpy( v, s_token );

		ASE_GetToken( false );
		strcpy( w, s_token );

		pMesh->tvertexes[index].s = atof( u );
		pMesh->tvertexes[index].t = atof( v );
	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_TVERTLIST" );
	}
}

void ASE_Loader::ASE_KeyMESH( const char *token )
{
	ASE_Mesh* pMesh = mCurrMesh;
	if ( !strcmp( token, "*MESH_NUMVERTEX" ) )
	{
		ASE_GetToken( false );

		pMesh->numVertexes = atoi( s_token );
		LOGI(  ".....num vertexes: %d\n", pMesh->numVertexes );
	}
	else if ( !strcmp( token, "*MESH_NUMFACES" ) )
	{
		ASE_GetToken( false );
		pMesh->numFaces = atoi( s_token );
		LOGI(  ".....num faces: %d\n", pMesh->numFaces );
	}
	else if ( !strcmp( token, "*MESH_NUMTVFACES" ) )
	{
		ASE_GetToken( false );
		if ( atoi( s_token ) != pMesh->numFaces )
		{
			LOGE( "MESH_NUMTVFACES != MESH_NUMFACES" );
		}
	}
	else if ( !strcmp( token, "*MESH_NUMTVERTEX" ) )
	{
		ASE_GetToken( false );

		pMesh->numTVertexes = atoi( s_token );
		LOGI( ".....num tvertexes: %d\n", pMesh->numTVertexes );
	}
	else if ( !strcmp( token, "*MESH_VERTEX_LIST" ) )
	{
		pMesh->vertexes = new ASE_Vertex[pMesh->numVertexes];//calloc( sizeof( aseVertex_t ) * pMesh->numVertexes, 1 );
		LOGI(  ".....parsing MESH_VERTEX_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_VERTEX_LIST );
	}
	else if ( !strcmp( token, "*MESH_TVERTLIST" ) )
	{
		pMesh->tvertexes = new ASE_TVertex[pMesh->numTVertexes];//calloc( sizeof( aseTVertex_t ) * pMesh->numTVertexes, 1 );
		LOGI(  ".....parsing MESH_TVERTLIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_TVERTLIST );
	}
	else if ( !strcmp( token, "*MESH_FACE_LIST" ) )
	{
		pMesh->faces = new ASE_Face[pMesh->numFaces]; //calloc( sizeof( aseFace_t ) * pMesh->numFaces, 1 );
		LOGI(  ".....parsing MESH_FACE_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_FACE_LIST );
	}
	else if ( !strcmp( token, "*MESH_TFACELIST" ) )
	{
		pMesh->tfaces = new ASE_Face[pMesh->numFaces];//calloc( sizeof( aseFace_t ) * pMesh->numFaces, 1 );
		LOGI(  ".....parsing MESH_TFACE_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyTFACE_LIST );
	}
	else if ( !strcmp( token, "*MESH_NORMALS" ) )
	{
		ASE_ParseBracedBlock( 0 );
	}
}

void ASE_Loader::ASE_KeyMESH_ANIMATION( const char *token )
{

}
void ASE_Loader::ASE_KeyNODETM(const char* token)
{
    if(!strcmp( token, "*TM_POS" ))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        mCurrGeomObject->translate[0] = x;
        mCurrGeomObject->translate[1] = y;
        mCurrGeomObject->translate[2] = z;
    }
    else if(!strcmp( token, "*TM_ROTAXIS" ))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        mCurrGeomObject->rotateAxis[0] = x;
        mCurrGeomObject->rotateAxis[1] = y;
        mCurrGeomObject->rotateAxis[2] = z;
    }
    else if(!strcmp( token, "*TM_ROTANGLE"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        mCurrGeomObject->rotateAngle = x * 180.0 / 3.1415926;
    }
    else if(!strcmp( token, "*TM_SCALE"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);

        mCurrGeomObject->scale[0] = x;
        mCurrGeomObject->scale[1] = y;
        mCurrGeomObject->scale[2] = z;
        
    }
    else if(!strcmp( token, "*TM_SCALEAXIS"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        mCurrGeomObject->scaleAxis[0] = x;
        mCurrGeomObject->scaleAxis[1] = y;
        mCurrGeomObject->scaleAxis[2] = z;

    }
    else if(!strcmp( token, "*TM_SCALEAXISANG"))
    {}
}
void ASE_Loader::ASE_KeyGEOMOBJECT( const char *token )
{
	if ( !strcmp( token, "*NODE_NAME" ) )
	{
		ASE_GetToken( true );
		LOGI(  " %s\n", s_token  );
		strcpy( mCurrGeomObject->name, s_token + 1 );
		if ( strchr( mCurrGeomObject->name, '"' ) )
			*strchr( mCurrGeomObject->name, '"' ) = 0;
		if(!strcmp(mCurrGeomObject->name , "Camera01"))
		{
		    LOGI("... has camera setting\n");
		}
	}
	else if ( !strcmp( token, "*NODE_PARENT" ) )
	{
		ASE_SkipRestOfLine();
	}
	// ignore unused data blocks
	else if ( !strcmp( token, "*TM_ANIMATION" ) )
	{
		ASE_ParseBracedBlock( 0 );
	}
	// ignore regular meshes that aren't part of animation
	else if ( !strcmp( token, "*MESH" ))
	{
        mCurrGeomObject->mesh = new ASE_Mesh;
        mCurrMesh =  mCurrGeomObject->mesh;
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH );
	}
	// according to spec these are obsolete
	else if ( !strcmp( token, "*MATERIAL_REF" ) )
	{
		ASE_GetToken( false );

		mCurrGeomObject->materialref = atoi( s_token );
	}
	// loads a sequence of animation frames
	else if ( !strcmp( token, "*NODE_TM" ) )
	{
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyNODETM );
	}
    else if(!strcmp(token, "*WIREFRAME_COLOR"))
    {
        ASE_GetToken(false);
        float r = atof(s_token);
        ASE_GetToken(false);
        float g = atof(s_token);
        ASE_GetToken(false);
        float b = atof(s_token);
        mCurrGeomObject->wireframeColor[0]= r;
        mCurrGeomObject->wireframeColor[1]= g;
        mCurrGeomObject->wireframeColor[2]= b;
    }

	// skip unused info
	else if ( !strcmp( token, "*PROP_MOTIONBLUR" ) ||
		      !strcmp( token, "*PROP_CASTSHADOW" ) ||
			  !strcmp( token, "*PROP_RECVSHADOW" ) )
	{
		ASE_SkipRestOfLine();
	}
}



/*
** ASE_Process
*/
void ASE_Loader::ASE_Process(  )
{
#ifdef DEBUG
    int geomCount = 0;
#endif
	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "*3DSMAX_ASCIIEXPORT" ) ||
			 !strcmp( s_token, "*COMMENT" ) )
		{
			ASE_SkipRestOfLine();
		}
		else if ( !strcmp( s_token, "*SCENE" ) )
		{
			ASE_SkipEnclosingBraces();
		}
		else if ( !strcmp( s_token, "*MATERIAL_LIST" ) )
		{
			LOGI( "MATERIAL_LIST\n");

			ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMATERIAL_LIST );
		}
		else if ( !strcmp( s_token, "*GEOMOBJECT" ) )
		{
			LOGI( "GEOMOBJECT\n"  );
                    ASE_GeometryObject *obj = new ASE_GeometryObject;
			mSceneObject->mGeomObjects.push_back(obj);
			mCurrGeomObject = obj;
			ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyGEOMOBJECT );
#ifdef DEBUG
			geomCount++;
#endif
	    }	
	}
#ifdef DEBUG
	LOGI(".. geomCount = %d \n", geomCount);
#endif
    ASE_AdjustSubMtl();
}
void ASE_Loader::ASE_AdjustSubMtl()
{
    GeomObjectList::iterator it;
    for(it = mSceneObject->mGeomObjects.begin() ; it != mSceneObject->mGeomObjects.end() ; it++)
    {
        ASE_GeometryObject* obj = *it;
        if(obj->materialref == -1)
            continue;
        ASE_Material* pMat = &mSceneObject->mMats[obj->materialref];
        if(pMat->submaterials != NULL)
        {
            int subMatlNum = pMat->numsubmaterials;
            for(int i = 0 ; i < obj->mesh->numFaces ; i++)
            {
                obj->mesh->faces[i].materialID = obj->mesh->faces[i].materialID % subMatlNum;
            }
            std::vector<int> faceGroupSet(subMatlNum, 0);
            for(int i = 0 ; i < obj->mesh->numFaces ; i++)
            {
                faceGroupSet[obj->mesh->faces[i].materialID]++;
            }
            obj->mesh->faceGroup.resize(subMatlNum);
            for(int i = 0 ; i < obj->mesh->numFaces; i++)
            {
                std::list<int>* l = &(obj->mesh->faceGroup[obj->mesh->faces[i].materialID]);
                l->push_back(i);
            } 
            for(int i = 0 ; i < subMatlNum ; i++)
            {
                if(faceGroupSet[i] > 0)
                    obj->mesh->numFaceGroup++;
            }
        }
    }
}
///////////////////////////////

