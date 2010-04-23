#include "aselib.h"
#include "../SE_Log.h"
#include "../SE_GeometryData.h"
#include "../SE_ResourceManager.h"
#include "../SE_Memory.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <list>
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
#define COPY_MATERIAL(type) do{ \
    dstm->materialData.type.x = srcm->materialData.type[0]; \
    dstm->materialData.type.y = srcm->materialData.type[1]; \
    dstm->materialData.type.z = srcm->materialData.type[2]; \
}while(0)
#define COPY_SUBMATERIAL(type) do{ \
    subdstm->type.x = subsrcm->type[0]; \
    subdstm->type.y = subsrcm->type[1]; \
    subdstm->type.z = subsrcm->type[2]; \
}while(0)
#define COPY_MESHP(type) do{\
    semesh->type.x = go->type[0]; \
    semesh->type.y = go->type[1]; \
    semesh->type.z = go->type[2]; \
}while(0)
static void writeString(const char* str, FILE* fout)
{
    int len = strlen(str);
    fwrite(&len, sizeof(int), 1, fout);
    fwrite(str, 1, len, fout);
}
static void getMeshes(ASE_SceneObject* mSceneObject, std::list<SE_Mesh*>& meshList)
{
    int geomDataNum = mSceneObject->mGeomObjects.size();
    std::list<ASE_GeometryObject*>::iterator it;
    int i = 0;
    for(it = mSceneObject->mGeomObjects.begin();
        it != mSceneObject->mGeomObjects.end();
        it++, i++)
    {
        ASE_GeometryObject* go = *it;
        ASE_Mesh* mesh = go->mesh;
        SE_Mesh* semesh = (SE_Mesh*)SE_Malloc(sizeof(SE_Mesh));
        meshList.push_back(semesh);
        SE_Object_Clear(semesh, sizeof(SE_Mesh));
        semesh->geomDataIndex = i;
        semesh->materialIndex = go->materialref;
        semesh->defaultColor.x = go->defaultColor[0];
        semesh->defaultColor.y = go->defaultColor[1];
        semesh->defaultColor.z = go->defaultColor[2];
        COPY_MESHP(rotateAxis);
        COPY_MESHP(scale);
        COPY_MESHP(scaleAxis);
        COPY_MESHP(translate);
        semesh->rotateAngle = go->rotateAngle;
        SE_String_Init(&semesh->name, go->name);
        if(mesh->numFaceGroup > 0)
        {
            semesh->subMeshArray = (SE_SubMesh*)SE_Malloc(sizeof(SE_SubMesh) * mesh->numFaceGroup);
            semesh->subMeshNum = mesh->numFaceGroup;
            int i = 0 ; 
            for(int j = 0 ; j < mesh->faceGroup.size() ; j++)
            {
                std::list<int>* l = &mesh->faceGroup[j];
                if(l->size() > 0)
                {
                    SE_SubMesh* submesh = &semesh->subMeshArray[i];
                    submesh->subMaterialIndex = j;
                    submesh->faceList.source = NULL;
                    submesh->faceList.num = l->size();
                    submesh->faceList.faces = (int*)SE_Malloc(submesh->faceList.num * sizeof(int));
                    std::list<int>::iterator it;
                    int k = 0;
                    for(it = l->begin() ; it != l->end() ; it++, k++)
                    {
                        submesh->faceList.faces[k] = *it;
                    }
                }
            }    
        }
    }
}
void ASE_Loader::Write(const char* filename)
{
    int materialNum = mSceneObject->mMats.size();
    SE_Material* materials = (SE_Material*)SE_Malloc(materialNum * sizeof(SE_Material));
    int i;
    for(i = 0 ; i < materialNum ; i++)
    {
        ASE_Material* srcm = &mSceneObject->mMats[i];
        SE_Material* dstm = &materials[i];
        COPY_MATERIAL(ambient);
        COPY_MATERIAL(diffuse);
        COPY_MATERIAL(specular);
        SE_String_Init(&dstm->materialData.texturename, srcm->materialData.texName);
        dstm->subMaterialNum = srcm->numsubmaterials;
        if(srcm->numsubmaterials > 0)
        {
            dstm->subMaterialArray = (SE_MaterialData*)SE_Malloc(sizeof(SE_MaterialData) * dstm->subMaterialNum);
            for(int j = 0 ; j < dstm->subMaterialNum ; j++)
            {
                SE_MaterialData* subdstm = &dstm->subMaterialArray[j];
                ASE_MaterialData* subsrcm = &srcm->submaterials[j];
                COPY_SUBMATERIAL(ambient);
                COPY_SUBMATERIAL(diffuse);
                COPY_SUBMATERIAL(specular);
                SE_String_Init(&subdstm->texturename, subsrcm->texName);
            } 
        }
    }
    int geomDataNum = mSceneObject->mGeomObjects.size();
    SE_GeometryData* geomDataArray = (SE_GeometryData*)SE_Malloc(geomDataNum * sizeof(SE_GeometryData));
    std::list<ASE_GeometryObject*>::iterator it;
    i = 0;
    for(it = mSceneObject->mGeomObjects.begin();
        it != mSceneObject->mGeomObjects.end();
        it++, i++)
    {
        ASE_GeometryObject* go = *it;
        SE_GeometryData* dstgd = &geomDataArray[i];
        ASE_Mesh* mesh = go->mesh;
        SE_Object_Clear(dstgd, sizeof(SE_GeometryData));
        dstgd->type = SE_TRIANGLES;
        dstgd->vertexNum = mesh->numVertexes;
        dstgd->vertexArray = (SE_Vector3f*)SE_Malloc(dstgd->vertexNum * sizeof(SE_Vector3f));
        int j;
        for(j = 0 ; j < dstgd->vertexNum ; j++ )
        {
            dstgd->vertexArray[j].x = mesh->vertexes[j].x;
            dstgd->vertexArray[j].y = mesh->vertexes[j].y;
            dstgd->vertexArray[j].z = mesh->vertexes[j].z;
        }
        dstgd->ownVertexArray = 1;
        dstgd->texVertexNum = mesh->numTVertexes;
        dstgd->texVertexArray = (SE_Vector3f*)SE_Malloc(dstgd->texVertexNum * sizeof(SE_Vector3f));
        for(j = 0 ; j < dstgd->texVertexNum ; j++)
        {
            dstgd->texVertexArray[j].x = mesh->tvertexes[j].s;
            dstgd->texVertexArray[j].y = mesh->tvertexes[j].t;
            dstgd->texVertexArray[j].z = 0;
        }
        dstgd->ownTexVertexArray = 1;
        dstgd->faceNum = mesh->numFaces;
        dstgd->faceArray = (SE_Face*)SE_Malloc(dstgd->faceNum * sizeof(SE_Face));
        for(j = 0 ; j < dstgd->faceNum ; j++)
        {
            dstgd->faceArray[j].v[0] = mesh->faces[j].vi[0];
            dstgd->faceArray[j].v[1] = mesh->faces[j].vi[1];
            dstgd->faceArray[j].v[2] = mesh->faces[j].vi[2];
        }
        dstgd->ownFaceArray = 1;
        if(mesh->tfaces)
        {
            dstgd->texFaceNum = mesh->numFaces;
            dstgd->texFaceArray = (SE_Face*)SE_Malloc(dstgd->texFaceNum * sizeof(SE_Face));
            for(j = 0 ; j < dstgd->texFaceNum ; j++)
            {
                dstgd->texFaceArray[j].v[0] = mesh->tfaces[j].vi[0];
                dstgd->texFaceArray[j].v[1] = mesh->tfaces[j].vi[1];
                dstgd->texFaceArray[j].v[2] = mesh->tfaces[j].vi[2];
            }
            dstgd->ownTexFaceArray = 1;
        }
    }
    std::list<SE_Mesh*> seMeshs;
    getMeshes(mSceneObject, seMeshs);
    int meshNum = seMeshs.size();
    FILE* fout = fopen(filename, "wb");
    if(!fout)
       return;
    fwrite(&MAGIC, sizeof(int), 1, fout);
    fwrite(&VERSION, sizeof(int), 1, fout);
    fwrite(&COORDINATE, sizeof(int), 1, fout);
    fwrite(&ENDIAN, sizeof(int), 1, fout);
    fwrite(&materialNum, sizeof(int), 1, fout);
    fwrite(&geomDataNum, sizeof(int), 1, fout);
    fwrite(&meshNum, sizeof(int), 1, fout);
    for(i = 0 ; i < materialNum ; i++)
    {
        LOGI("...write material\n");
        fwrite(&MATERIAL_ID, sizeof(short), 1, fout);
        SE_Material* m = &materials[i];
        writeString(SE_String_GetData(&m->materialData.texturename), fout);
        fwrite(&m->materialData.ambient.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.ambient.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.ambient.z, sizeof(float), 1, fout);
        
        fwrite(&m->materialData.diffuse.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.diffuse.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.diffuse.z, sizeof(float), 1, fout);
        
        fwrite(&m->materialData.specular.x, sizeof(float), 1, fout);
        fwrite(&m->materialData.specular.y, sizeof(float), 1, fout);
        fwrite(&m->materialData.specular.z, sizeof(float), 1, fout);
        fwrite(&m->subMaterialNum , sizeof(int), 1, fout);
        if(m->subMaterialNum > 0)
        {
            for(int j = 0 ; j < m->subMaterialNum ; j++)
            {
                SE_MaterialData* md = &m->subMaterialArray[j];
                writeString(SE_String_GetData(&md->texturename), fout);
                fwrite(&md->ambient.x, sizeof(float), 1, fout);
                fwrite(&md->ambient.y, sizeof(float), 1, fout);
                fwrite(&md->ambient.z, sizeof(float), 1, fout);
                
                fwrite(&md->diffuse.x, sizeof(float), 1, fout);
                fwrite(&md->diffuse.y, sizeof(float), 1, fout);
                fwrite(&md->diffuse.z, sizeof(float), 1, fout);
                
                fwrite(&md->specular.x, sizeof(float), 1, fout);
                fwrite(&md->specular.y, sizeof(float), 1, fout);
                fwrite(&md->specular.z, sizeof(float), 1, fout);
     
            }
        }

    }
    for(i = 0 ; i < geomDataNum ; i++)
    {
        LOGI("... write goem data\n");
        fwrite(&GEOMOBJECT_ID, sizeof(short), 1, fout);
        SE_GeometryData* gd = &geomDataArray[i];
        fwrite(&gd->type, sizeof(int), 1, fout);
        fwrite(&gd->vertexNum, sizeof(int), 1, fout);
        fwrite(&gd->faceNum, sizeof(int), 1, fout);
        fwrite(&gd->texVertexNum, sizeof(int), 1, fout);
        fwrite(&gd->colorNum, sizeof(int), 1, fout);
        int j;
        for(j = 0 ; j < gd->vertexNum ; j++)
        {
            fwrite(&gd->vertexArray[j].x, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].y, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].z, sizeof(float), 1, fout);
        }
        for(j = 0 ; j < gd->faceNum ; j++)
        {
            fwrite(&gd->faceArray[j].v[0], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[1], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[2], sizeof(int), 1, fout);

        }
        if(gd->texVertexNum > 0)
        {
            for(j = 0 ; j < gd->texVertexNum ; j++)
            {
                fwrite(&gd->texVertexArray[j].x, sizeof(float), 1, fout);
                fwrite(&gd->texVertexArray[j].y, sizeof(float), 1, fout);
                fwrite(&gd->texVertexArray[j].z, sizeof(float), 1, fout);
            }
            for(j = 0 ; j < gd->faceNum ; j++)
            {
                fwrite(&gd->texFaceArray[j].v[0], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[1], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[2], sizeof(int), 1, fout);
            }
        }
        if(gd->colorNum > 0)
        {
            for(j = 0 ; j < gd->colorNum ; j++)
            {
                fwrite(&gd->colorArray[j].x, sizeof(float), 1, fout);
                fwrite(&gd->colorArray[j].y, sizeof(float), 1, fout);
                fwrite(&gd->colorArray[j].z, sizeof(float), 1, fout);
            }
        }
    }
    i = 0;
    std::list<SE_Mesh*>::iterator itMesh;
    for(itMesh = seMeshs.begin() ; itMesh != seMeshs.end() ; itMesh++)
    {
        SE_Mesh* smesh = *itMesh;
        LOGI("... write mesh id\n");
        fwrite(&MESH_ID, sizeof(short), 1, fout);
        fwrite(&smesh->geomDataIndex, sizeof(int), 1, fout);
        fwrite(&smesh->materialIndex, sizeof(int), 1, fout);
        fwrite(&smesh->defaultColor.x, sizeof(float), 1, fout);
        fwrite(&smesh->defaultColor.y, sizeof(float), 1, fout);
        fwrite(&smesh->defaultColor.z, sizeof(float), 1, fout);

        fwrite(&smesh->rotateAxis.x, sizeof(float), 1, fout);
        fwrite(&smesh->rotateAxis.y, sizeof(float), 1, fout);
        fwrite(&smesh->rotateAxis.z, sizeof(float), 1, fout);

        fwrite(&smesh->rotateAngle, sizeof(float), 1, fout);

        fwrite(&smesh->scaleAxis.x, sizeof(float), 1, fout);
        fwrite(&smesh->scaleAxis.y, sizeof(float), 1, fout);
        fwrite(&smesh->scaleAxis.z, sizeof(float), 1, fout);

        fwrite(&smesh->scale.x, sizeof(float), 1, fout);
        fwrite(&smesh->scale.y, sizeof(float), 1, fout);
        fwrite(&smesh->scale.z, sizeof(float), 1, fout);

        fwrite(&smesh->translate.x, sizeof(float), 1, fout);
        fwrite(&smesh->translate.y, sizeof(float), 1, fout);
        fwrite(&smesh->translate.z, sizeof(float), 1, fout);
        writeString(SE_String_GetData(&smesh->name), fout);
        fwrite(&smesh->subMeshNum, sizeof(int), 1, fout);
        if(smesh->subMeshNum > 0)
        {
            int i;
            for(i = 0 ; i < smesh->subMeshNum ; i++)
            {
                SE_SubMesh* submesh = &smesh->subMeshArray[i];
                fwrite(&submesh->subMaterialIndex, sizeof(int), 1, fout);
                fwrite(&submesh->faceList.num, sizeof(int), 1, fout);
                int j;
                for(j = 0 ; j < submesh->faceList.num ; j++)
                {
                    fwrite(&submesh->faceList.faces[j], sizeof(int), 1, fout);
                }
            }
        }

    } 
    LOGI("write end\n");
    fclose(fout); 
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
        mCurrGeomObject->rotateAngle = x;
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
        mCurrGeomObject->defaultColor[0]= r;
        mCurrGeomObject->defaultColor[1]= g;
        mCurrGeomObject->defaultColor[2]= b;
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

	    }	
	}
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
            /*
            subMatList.sort();
            std::vector<int> subMatlArray(subMatList.size());
            copy(subMatList.begin(), subMatList.end(), subMatList.begin());
            for(int i = 0 ; i < subMatlArray.size() ; i++)
            {
                int index = i % subMatlNum;
                for(int j = 0 ; j < obj->mesh->numFaces; j++)
                {
                    if(obj->mesh->faces[j].materialID == subMatlArray[i])
                    {
                        obj->mesh->faces[j].materialID = index;
                    }
                }
            }
            */
        }
    }
}
///////////////////////////////
/*
ASE_Mesh* ASE_Loader::getMesh(int idx)
{
	ASE_Mesh *pMesh = NULL;
	std::list<ASE_Mesh*>::iterator it;
	int i = 0;
	for(it = mGeometryObject->meshList.begin() ;
		it != mGeometryObject->meshList.end(); it++)
	{
		pMesh = *it;
        if(i == idx)
		{
			break;
		}
		else
		{
			i++;
		}
	}
	if(it == mGeometryObject->meshList.end())
	{
		return NULL;
	}
	else
	{
	    return pMesh;
	}
}

int ASE_Loader::getMeshCount()
{
	return mGeometryObject->meshList.size();
}

int ASE_Loader::createRenderList()
{
    int meshCount = getMeshCount();
	int ret = ASE_ERROR;
	for(int i = 0 ; i < meshCount ; i++)
	{
		ASE_Mesh* pMesh = getMesh(i);
        ret = createRenderMesh(pMesh, i);
		if(ret != ASE_OK)	
		{
			break;
		}
	}
	return ret;
}
int ASE_Loader::isFoundMaterial(ASE_MaterialData* pMaterialData, ASE_Render_Data*& pRenderData)
{
	std::list<ASE_Render_Data*>::iterator it;
	for(it = mRenderList->renderList.begin(); 
		it != mRenderList->renderList.end() ; it++)
	{
		ASE_Render_Data* renderData = *it;
		if(strcmp(renderData->materialData.texName, pMaterialData->texName) == 0)
		{
			pRenderData = renderData;
			return true;
		}
	}
	return false;
}
int ASE_Loader::isMeshInRenderFaceList(std::list<ASE_Render_Face*> * pRenderFaceList,
										  int meshIndex, ASE_Render_Face*& pRenderFace)
{
	std::list<ASE_Render_Face*>::iterator it;
	for(it = pRenderFaceList->begin() ; 
		it != pRenderFaceList->end() ; it++)
	{
		ASE_Render_Face* pFace = *it;
		if(pFace->meshIndex == meshIndex)
		{
			pRenderFace = pFace;
			return true;
		}
	}
	return false;
}
int ASE_Loader::createRenderMesh(ASE_Mesh* pMesh, int meshIndex)
{
	int materialref = pMesh->materialref;
	ASE_Material material = mMtlList[materialref];
	if(material.numsubmaterials == 0)
	{
		ASE_Render_Data* pFoundRenderData = NULL;
		if(isFoundMaterial(&material.materialData, pFoundRenderData))
		{
            assert(pFoundRenderData != NULL);
			ASE_Render_Face* pRenderFace = new ASE_Render_Face;
			pRenderFace->meshIndex = meshIndex;
			pFoundRenderData->renderFaceList.push_back(pRenderFace);
		}
		else
		{
			ASE_Render_Data *pRenderData = new ASE_Render_Data;
			pRenderData->materialData = material.materialData;
			ASE_Render_Face* pRenderFace = new ASE_Render_Face;
			pRenderFace->meshIndex = meshIndex;
			pRenderData->renderFaceList.push_back(pRenderFace);
			if(strcmp(pRenderData->materialData.texName, "") == 0)
			{
				return ASE_ERROR;
			}
			mRenderList->renderList.push_back(pRenderData);
		}
	}
	else
	{
		int numFace = pMesh->numFaces;
		for(int i = 0 ; i < numFace ; i++)
		{
			ASE_Face face = pMesh->faces[i];
			if(strcmp(pMesh->name , "Object08") == 0)
			{
				char* str = pMesh->name;
			}
			int materialID = face.materialID;
			if(materialID >= material.numsubmaterials)
			{
				int tmpMaterialID = materialID % material.numsubmaterials;
				if(tmpMaterialID == 0)
				{
					materialID = 0;
				}
				else
				{
					materialID = tmpMaterialID - 1;
				}
				
			}
			ASE_MaterialData submaterial = material.submaterials[materialID];
            ASE_Render_Data* pRenderData = NULL;
            if(isFoundMaterial(&submaterial, pRenderData))
			{
				std::list<ASE_Render_Face*> *pRenderFaceList = &pRenderData->renderFaceList;
				ASE_Render_Face* pRenderFace = NULL;
                if(isMeshInRenderFaceList(pRenderFaceList, meshIndex, pRenderFace))
				{
					pRenderFace->faceIndexList.push_back(i);
				}
				else
				{
					pRenderFace = new ASE_Render_Face;
					pRenderFace->meshIndex = meshIndex;
					pRenderFace->faceIndexList.push_back(i);
					pRenderFaceList->push_back(pRenderFace);
				}
			}
			else
			{
			    pRenderData = new ASE_Render_Data;
				pRenderData->materialData = submaterial;
				ASE_Render_Face* pRenderFace = new ASE_Render_Face;
				pRenderFace->meshIndex = meshIndex;
				pRenderFace->faceIndexList.push_back(i);
				pRenderData->renderFaceList.push_back(pRenderFace);
				if(strcmp(pRenderData->materialData.texName, "") == 0)
			    {
					char* str = pMesh->name;
				    return ASE_ERROR;
			    }
				mRenderList->renderList.push_back(pRenderData);
			}
		}
	}
	return ASE_OK;
}
*/
