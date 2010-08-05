#include "SE_Ase.h"
#include "SE_Log.h"
#include "SE_GeometryData.h"
#include "../SE_ResourceManager.h"
#include "../SE_Memory.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
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


ASE_Loader::ASE_Loader(const char* filename, bool verbose, bool meshanims) : mCurrGeomObject(NULL), mCurrMtl(NULL),mCurrSubMtl(NULL),mCurrMesh(NULL), mInSubDiffuse(false)
{
    mSceneObject = new ASE_SceneObject;
    _verbose = verbose;
    strncpy(meshFileName, filename, 256);
}
ASE_Loader::~ASE_Loader()
{
    delete mSceneObject;
}
void ASE_Loader::Load()
{
    ASE_Load(meshFileName, _verbose);

}
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
    SE_TextureCoordDataiD texCoordID;
};
void ASE_Loader::Write(SE_BufferOutput& output)
{
    int materialNum = mSceneObject->mMats.size();
    std::vector<_MaterialData> materialVector(materialNum);
    std::vector<int> indexWhichHasSubmaterial;
    int i;
    for(i = 0 ; i < materialNum ; i++)
    {
        ASE_Material* srcm = &mSceneObject->mMats[i];
        _MaterialData md;
        md.subMaterialNum = srcm->numsubmaterials;
        md.md = srcm->materialData;
        materialVector.push_back(md);
        if(srcm->numsubmaterials > 0)
        {
            indexWhichHasSubmaterial.push_back(i);
        }
    }
    std::vector<int>::iterator it;
    for(it = indexWhichHasSubmaterial.begin() ; it != indexWhichHasSubmateria.end() ; it++)
    {
        int index = *it;
        ASE_Material* m = &mSceneObject->mMats[index];
        for(int j = 0 ; j < m->numsubmaterials ; j++)
        {
            _MaterialData md;
            md.subMaterialNum = 0;
            md.md = m->submaterials[j];
            materialVector.push_back(md);
        }
    }
    std::vector<_MaterialData>::iterator itMaterial;
    output.writeShort(SE_MATERIALDATA_ID);
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        SE_MaterialDataID mid = SE_Application::getInstance()->createCommonID();
        itMaterial->mid = mid;
        sleep(1);
        mid.write(output);
        output.write(itMaterial->md.ambient);
        output.write(itMaterial->md.diffuse);
        output.write(itMaterial->md.specular);
        output.write(SE_Vector3f(0, 0, 0));
    }
    /////////////////////////////write texture data ///////////////
    output.writeShort(SE_IMAGEDATA_ID);
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        std::string texStr(itMaterial->md.texName);
        if(texStr != "")
        {
            SE_ImageDataID tid = SE_Application::getInstance()->createCommonID();
            itMaterial->tid = tid;
            sleep(1);
            tid.write(output);
            output.writeInt(0); // image data type
            output.writeString(texStr.c_str());
        }
    }
    /////////////////////////////write geom data /////////////////////////////////////////////
    output.writeShort(SE_GEOMETRYDATA_ID);
    int geomDataNum = mSceneObject->mGeomObjects.size();
    output.writeInt(geomDataNum);
    std::vector<_GeomTexCoordData> geomTexCoordData(geomDataNum);
    std::list<ASE_GeometryObject*>::iterator it;
    int n = 0;
    for(it = mSceneObject->mGeomObjects.begin();
        it != mSceneObject->mGeomObjects.end();
        it++)
    {
        ASE_GeometryObject* go = *it;
        ASE_Mesh* mesh = go->mesh;
        SE_GeometryDataID gid = SE_Application::getInstance()->createCommonID();
        geomTexCoorData[n++].geomID = gid;
        gid.write(output);
        output.writeInt(mesh->numVertexes);
        output.writeInt(mesh->numFaces);
        output.writeInt(0);
        int i;
        for(i = 0 ; i < mesh->numVertexes ; i++)
        {
            output.writeFloat(mesh->vertexes[i].x);
            output.writeFloat(mesh->vertexes[i].y);
            output.writeFloat(mesh->vertexes[i].z);
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
    n = 0;
    for(it = mSceneObject->mGeomObjects.begin();
    it != mSceneObject->mGeomObjects.end();
    it++)
    {
        ASE_GeometryObject* go = *it;
        ASE_Mesh* mesh = go->mesh;
        SE_TextureCoordDataID tcid = SE_Application::getInstance()->createCommonID();
        tcid.write(tcid);
        geomTexCoordData[n++].texCoordID = tcid;
        sleep(1);
        output.write(mesh->numTVertexes);
        output.write(mesh->numFaces);
        int i;
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
///////////////////// write mesh //////////////// 
    output.writeShort(SE_MESHDATA_ID);
    output.writeInt(geomDataNum);
    n = 0;
    for(it = mSceneObject->mGeomObjects.begin();
    it != mSceneObject->mGeomObjects.end();
    it++)
    {
        ASE_GeometryObject* go = *it;
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = SE_Application::getInstance()->createCommonID();
        sleep(1);
        meshID.write(output);
        geomTexCoordData[n++].geomID.write(output);
        output.writeFloat(wireframeColor[0]);
        output.writeFloat(wireframeColor[1]);
        output.writeFloat(wireframeColor[2]);
        int texNum = 0;
        int materiaref = go->materialref;
        _MaterialData mdData = materialVector[materialref];
        if(mdData.subMaterialNum > 1)
        {
            int startpos = 0;
            int j;
            for(j = 0 ; j < (materialref - 1) ; j++)
            {
                _MaterialData d = materialVector[j];
                startpos += d.subMaterialNum;
            }
            for(int j = 0 ; j < mdData.subMaterialNum ; j++)
            {
                _MaterialData subMaterialData = materialVector[startpos++];
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

        }
    }

    LOGI("write end\n");
}
/*
** ASE_Load
*/
void ASE_Loader::ASE_Load( const char *filename, bool verbose)
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

