#include "aselib.h"
#include "SE_Log.h"
#include "SE_GeometryData.h"
#include "SE_Memory.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Math.h"
#include "SE_Quat.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <list>
#include <assert.h>
template <typename T>
typename std::list<T>::iterator se_list_nref(std::list<T>& data, size_t n)
{
    typename std::list<T>::iterator it = data.begin();
    size_t i = 0;
    while(it != data.end() && i < n)
    {
        it++;
        i++;
    }
    return it;
}
static std::vector<std::string> splitString(const char* path, const char* split)
{
    std::list<std::string> retList;
    std::vector<std::string> ret;
    if(!path)
        return ret;
    if(!split)
    {
        ret.resize(1);
        ret[0] = path;
        return ret;
    }
    std::string str = path;
    std::string strSplit = split;
    std::string::size_type pos = 0;
    std::string::size_type start = 0;
    while(pos < str.size())
    {
        pos = str.find_first_of(strSplit, start);
        if(pos != std::string::npos)
        {
            std::string::size_type n = pos - start;
            if(n > 0)
            {
                std::string subStr = str.substr(start, n);
                retList.push_back(subStr);
            }
            start = pos + 1;
        }
        else
        {
            std::string subStr = str.substr(start);
			if(subStr != "")
                retList.push_back(subStr);
            pos = str.size();
        }
    }
    if(retList.empty())
    {
        retList.push_back(path);
    }
    ret.resize(retList.size());
    std::list<std::string>::iterator it;
    int i = 0;
    for(it = retList.begin() ; it != retList.end() ;it++)
    {
        ret[i++] = *it;
    }
    return ret;
    
}

static size_t getFileLen(FILE* fp)
{
	size_t		pos;
	size_t		end;

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
static const short SHADER_ID = 0x0007;
static const short REFERENCE_BOX_ID = 0x0008;
static const short TRACK_POINT_ID = 0x0009;
static const short LOOKING_POINT_ID = 0x0010;
static const short LOOKING_POINT_TRACK_ID = 0x0011;
static const short VERTICAL_TRACK_POINT_ID = 0x0012;
ASE_Loader::ASE_Loader(bool verbose, bool meshanims) : mCurrGeomObject(NULL), mCurrMtl(NULL),mCurrSubMtl(NULL),mCurrMesh(NULL), mInSubDiffuse(false)
{
    mSceneObject = new ASE_SceneObject;
    _verbose = verbose;
    mCurrentMapChannel = NULL;
    mGeomObjComplement = false;
    mTrackListPhotoTypeMap["vv"] = VV;
    mTrackListPhotoTypeMap["vh"] = VH;
    mTrackListPhotoTypeMap["hv"] = HV;
    mTrackListPhotoTypeMap["hh"] = HH;
}
ASE_Loader::~ASE_Loader()
{
    delete mSceneObject;
    
}
void ASE_Loader::Load(const char* filename)
{
    ASE_Load(filename, _verbose);
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
static void writeString(const char* str, int len, FILE* fout)
{
    fwrite(&len, 1, sizeof(int), fout);
    fwrite(str, 1, len, fout);
}
static void writeString(const char* str, FILE* fout, bool hasNullEnd = false)
{
    size_t len = strlen(str);
    if(len > 0)
    {
        char buf[256];
        memset(buf, 0, 256);
        strncpy(buf, str, 255);
        LOGI("### string = %s ####\n", buf);
        len = strlen(buf);
        if(hasNullEnd)
        {
            len = len + 1;
            fwrite(&len, sizeof(int), 1, fout);
            fwrite(buf, 1, len , fout);
        }
        else
        {
            fwrite(&len, sizeof(int), 1, fout);
            fwrite(buf, 1, len, fout);
        }
    }
    else
    {
        fwrite(&len, sizeof(int), 1, fout);
    }
}
static void getMeshes(ASE_SceneObject* mSceneObject, std::list<SE_Mesh*>& meshList)
{
    size_t geomDataNum = mSceneObject->mGeomObjects.size();
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
        semesh->wireframeColor.x = go->wireframeColor[0];
        semesh->wireframeColor.y = go->wireframeColor[1];
        semesh->wireframeColor.z = go->wireframeColor[2];
        COPY_MESHP(rotateAxis);
        COPY_MESHP(scale);
        COPY_MESHP(scaleAxis);
        COPY_MESHP(translate);
        semesh->rotateAngle = go->rotateAngle;
        SE_String_Init(&semesh->name, go->name);
        LOGI("... numFaceGroup = %d\n", mesh->numFaceGroup);
        if(mesh->numFaceGroup > 0)
        {
            semesh->subMeshArray = (SE_SubMesh*)SE_Malloc(sizeof(SE_SubMesh) * mesh->numFaceGroup);
            semesh->subMeshNum = mesh->numFaceGroup;
            for(int j = 0 ; j < mesh->faceGroup.size() ; j++)
            {
                std::list<int>* l = &mesh->faceGroup[j];
                if(l->size() > 0)
                {
                    SE_SubMesh* submesh = &semesh->subMeshArray[j];
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
static void check(SE_GeometryData* gd, int i)
{
        LOGI("## check : %d\n", i);
        for(int j = 0 ;j < gd->faceNum; j++)
        {
            SE_Face f = gd->faceArray[j];
            SE_Vector3f p0, p1, p2;
            p0 = gd->vertexArray[f.v0];
            p1 = gd->vertexArray[f.v1];
            p2 = gd->vertexArray[f.v2];
            SE_Vector3f v0, v1;
            SE_Vec3f_Subtract(&p1, &p0, &v0);
            SE_Vec3f_Subtract(&p2, &p0, &v1);
            SE_Vector3f cross;
            SE_Vec3f_Cross(&v0, &v1, &cross);
            SE_Vector3f n = gd->normalArray[j];
            float d = SE_Vec3f_Dot(&n, &cross);
            if(d < 0 )
            {
                LOGI("## normal reverse : %d ###\n", j);
            }
        }
}
void ASE_Loader::calculateReferenceBox()
{
    ASE_GeometryObject* geomObj = mReferenceBox.reference;
    ASE_Mesh* mesh = geomObj->mesh;
    ASE_Vertex min = ASE_Vertex(1000, 1000, 1000);
    ASE_Vertex max = ASE_Vertex(-1000, -1000, -1000);
    for(int i = 0 ; i < mesh->numVertexes ; i++)
    {
        ASE_Vertex v = mesh->vertexes[i];
        if(v.x < min.x)
            min.x = v.x;
        if(v.y < min.y)
            min.y = v.y;
        if(v.z < min.z)
            min.z = v.z;
        if(v.x > max.x)
            max.x = v.x;
        if(v.y > max.y)
            max.y = v.y;
        if(v.z > max.z)
            max.z = v.z;
    }
    mReferenceBox.min = min;
    mReferenceBox.max = max;
}
struct _VertexData
{
    SE_Vertex_XYZUV xyzuv;
    int index;
};
typedef std::list<_VertexData> _VertexDataList;
struct _GeomDataMap
{
    SE_Vector3f v;
    std::list<_VertexDataList::iterator> vertexDataIndexList;
};
struct _FaceData
{
    std::list<_VertexData>::iterator v[3];
};
static void setVertex(std::list<_VertexData>& vertexDataList, _FaceData& faceData, SE_GeometryData* geomData, std::vector<_GeomDataMap>& geomDataMapVector, SE_Face& geomFace, SE_Face& texFace, int i)
{
    _GeomDataMap* gdm = &geomDataMapVector[geomFace.v[i]];
    assert(geomData->vertexArray[geomFace.v[i]].x == geomDataMapVector[geomFace.v[i]].v.x);
    assert(geomData->vertexArray[geomFace.v[i]].y == geomDataMapVector[geomFace.v[i]].v.y);
    assert(geomData->vertexArray[geomFace.v[i]].z == geomDataMapVector[geomFace.v[i]].v.z);
    bool found = false;
    std::list<_VertexDataList::iterator>::iterator it;
    for(it = gdm->vertexDataIndexList.begin() ; it != gdm->vertexDataIndexList.end() ; it++)
    {
        
        float deltau = SE_Fabs((*it)->xyzuv.u - geomData->texVertexArray[texFace.v[i]].x);
        float deltav = SE_Fabs((*it)->xyzuv.v - geomData->texVertexArray[texFace.v[i]].y);
        if(deltau < 0.001 && deltav < 0.001)
        {
            faceData.v[i] = *it;
            found = true;
            break;
        }
    }
    if(found == false)
    {
        _VertexData vd;
        int index = vertexDataList.size();
        vd.xyzuv.x = gdm->v.x;
        vd.xyzuv.y = gdm->v.y;
        vd.xyzuv.z = gdm->v.z;
        vd.xyzuv.u = geomData->texVertexArray[texFace.v[i]].x;
        vd.xyzuv.v = geomData->texVertexArray[texFace.v[i]].y;
        vd.index = index;
        vertexDataList.push_back(vd);
        faceData.v[i] = se_list_nref(vertexDataList, vertexDataList.size() - 1);
        gdm->vertexDataIndexList.push_back(faceData.v[i]);
    }
    
}
SE_VertexBuffer ASE_Loader::createVertexBuffer(SE_GeometryData* geomData)
{
    std::list<_VertexData> vertexDataList;
    std::vector<_FaceData> faceDataArray(geomData->faceNum);
    std::vector<_GeomDataMap> geomDataMapVector(geomData->vertexNum);
    for(int i = 0 ; i < geomData->vertexNum ; i++)
    {
        geomDataMapVector[i].v = geomData->vertexArray[i];
    }
    int faceNum = geomData->faceNum;
    std::vector<SE_Face> faceData(faceNum);
    for(int i = 0 ;i < faceNum ; i++)
    {
        SE_Face geomFace = geomData->faceArray[i];
        SE_Face texFace = geomData->texFaceArray[i];
        _FaceData faceData;
        
        setVertex(vertexDataList, faceData, geomData, geomDataMapVector, geomFace, texFace, 0);
        setVertex(vertexDataList, faceData, geomData, geomDataMapVector, geomFace, texFace, 1);
        setVertex(vertexDataList, faceData, geomData, geomDataMapVector, geomFace, texFace, 2);
        faceDataArray[i] = faceData;
    }
    std::list<_VertexData>::iterator it;
    for(it = vertexDataList.begin() ; it != vertexDataList.end() ; it++)
    {
        LOGI("%d ", it->index);
    }
    LOGI("\n");
    SE_VertexBuffer vertexBuffer;
    vertexBuffer.vertexNum = vertexDataList.size();
    vertexBuffer.vertexArray = new SE_Vertex_XYZUV[vertexBuffer.vertexNum];
    int i = 0;
    for(i = 0, it = vertexDataList.begin() ; it != vertexDataList.end() ; it++, i++)
    {
        vertexBuffer.vertexArray[i] = it->xyzuv;
    }
    vertexBuffer.faceNum = faceNum;
    vertexBuffer.faceArray = new int[faceNum * 3];
    int j = 0;
    for(i = 0 ; i < faceNum ; i++)
    {
        _FaceData fd = faceDataArray[i];
        vertexBuffer.faceArray[j++] = fd.v[0]->index;
        vertexBuffer.faceArray[j++] = fd.v[1]->index;
        vertexBuffer.faceArray[j++] = fd.v[2]->index;
    }
    return vertexBuffer;
}
void ASE_Loader::Write(const char* filename)
{
    calculateReferenceBox();
    int materialNum = mSceneObject->mMats.size();
    SE_Material* materials = (SE_Material*)SE_Malloc(materialNum * sizeof(SE_Material));
    SE_Object_Clear(materials, sizeof(materialNum * sizeof(SE_Material)));
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
        SE_Matrix3f rotate, rs;
        SE_Matrix4f transformMatrix, invertTransformMatrix;
        SE_Vector3f rotateAxis, translate, scale;
        SE_Quat rotateQuat;
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        SE_Quat_InitFromAngleAxis(go->rotateAngle, &rotateAxis, &rotateQuat);
        SE_CreateTransformByRST(&rotateQuat, &scale, &translate, &transformMatrix);
        SE_Mat4f_Inverse(&transformMatrix, &invertTransformMatrix);
        int j;
        for(j = 0 ; j < dstgd->vertexNum ; j++ )
        {
            SE_Vector4f v1, v2;
            v1.x = mesh->vertexes[j].x;
            v1.y =  mesh->vertexes[j].y;
            v1.z = mesh->vertexes[j].z;
            v1.w = 1.0f;
            SE_Mat4f_Map(&invertTransformMatrix, &v1, &v2);
            SE_ASSERT(v2.w == 1.0f);
            dstgd->vertexArray[j].x = v1.x;
            dstgd->vertexArray[j].y = v1.y;
            dstgd->vertexArray[j].z = v1.z;
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
        if(mesh->tvertexes2)
        {
            dstgd->texVertexNum2 = mesh->numTVertexes2;
            dstgd->texVertexArray2 = (SE_Vector3f*)SE_Malloc(dstgd->texVertexNum2 * sizeof(SE_Vector3f));
            for(j = 0 ; j < dstgd->texVertexNum2 ; j++)
            {
                dstgd->texVertexArray2[j].x = mesh->tvertexes2[j].s;
                dstgd->texVertexArray2[j].y = mesh->tvertexes2[j].t;
                dstgd->texVertexArray2[j].z = 0;
            }
            dstgd->ownTexVertexArray2 = 1;
        }
        if(mesh->tfaces2)
        {
            dstgd->texFaceNum2 = mesh->numFaces;
            dstgd->texFaceArray2 = (SE_Face*)SE_Malloc(dstgd->texFaceNum2 * sizeof(SE_Face));
            for(j = 0 ; j < dstgd->texFaceNum2 ; j++)
            {
                dstgd->texFaceArray2[j].v[0] = mesh->tfaces2[j].vi[0];
                dstgd->texFaceArray2[j].v[1] = mesh->tfaces2[j].vi[1];
                dstgd->texFaceArray2[j].v[2] = mesh->tfaces2[j].vi[2];
            }
            dstgd->ownTexFaceArray2 = 1;
        }
        dstgd->normalNum = mesh->normalList.size();
        dstgd->normalArray = (SE_Vector3f*)SE_Malloc(dstgd->normalNum * sizeof(SE_Vector3f));
        j = 0;
        std::list<ASE_Vertex>::iterator itNormal;
        for(itNormal = mesh->normalList.begin() ; itNormal != mesh->normalList.end() ; itNormal++)
        {
            dstgd->normalArray[j].x = itNormal->x;
            dstgd->normalArray[j].y = itNormal->y;
            dstgd->normalArray[j].z = itNormal->z;
            j++;
        }
        check(dstgd, i);
        dstgd->ownMapChannel = 1;
        dstgd->numMapChannel = mesh->mapChannels.size();
        if(dstgd->numMapChannel > 0)
        {
            dstgd->mapChannelArray = (SE_MapChannel*)malloc(sizeof(SE_MapChannel) * dstgd->numMapChannel);
            
        }
        else
        {
            dstgd->mapChannelArray = NULL;
        }
        j = 0;
        std::list<ASE_MeshMapChannel*>::iterator itMapChannel;
        for(itMapChannel = mesh->mapChannels.begin() ; itMapChannel != mesh->mapChannels.end(); itMapChannel++)
        {
            ASE_MeshMapChannel* pMapChannel = *itMapChannel;
            dstgd->mapChannelArray[j].numTVertex = pMapChannel->numTVertex;
            dstgd->mapChannelArray[j].numTFaces = pMapChannel->numTFace;
            dstgd->mapChannelArray[j].texVertexArray = (SE_Vector2f*)malloc(sizeof(SE_Vector2f) * dstgd->mapChannelArray[j].numTVertex);
            dstgd->mapChannelArray[j].texFaceArray = (SE_Face*)malloc(sizeof(SE_Face) * dstgd->mapChannelArray[j].numTFaces);
            for(int k = 0 ; k < dstgd->mapChannelArray[j].numTVertex ; k++)
            {
                dstgd->mapChannelArray[j].texVertexArray[k].x = pMapChannel->tvertexes[k].s;
                dstgd->mapChannelArray[j].texVertexArray[k].y = pMapChannel->tvertexes[k].t;
            }
            for(int k = 0 ; k < dstgd->mapChannelArray[j].numTFaces ; k++)
            {
                dstgd->mapChannelArray[j].texFaceArray[k].v0 = pMapChannel->tfaces[k].vi[0];
                dstgd->mapChannelArray[j].texFaceArray[k].v1 = pMapChannel->tfaces[k].vi[1];
                dstgd->mapChannelArray[j].texFaceArray[k].v2 = pMapChannel->tfaces[k].vi[2];
            }
            j++;
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
        SE_VertexBuffer vertexBuffer = createVertexBuffer(gd);
        fwrite(&gd->type, sizeof(int), 1, fout);
        fwrite(&gd->vertexNum, sizeof(int), 1, fout);
        fwrite(&gd->faceNum, sizeof(int), 1, fout);
        fwrite(&gd->texVertexNum, sizeof(int), 1, fout);
        fwrite(&gd->texVertexNum2, sizeof(int), 1, fout);
        fwrite(&gd->colorNum, sizeof(int), 1, fout);
        fwrite(&vertexBuffer.vertexNum, sizeof(int), 1, fout);
        fwrite(&vertexBuffer.faceNum, sizeof(int), 1, fout);
        int j;
        for(j = 0 ; j < gd->vertexNum ; j++)
        {
            fwrite(&gd->vertexArray[j].x, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].y, sizeof(float), 1, fout);
            fwrite(&gd->vertexArray[j].z, sizeof(float), 1, fout);
            LOGI("vertex %d: %f , %f, %f\n", j, gd->vertexArray[j].x, gd->vertexArray[j].y, gd->vertexArray[j].z);
        }
        for(j = 0 ; j < gd->faceNum ; j++)
        {
            fwrite(&gd->faceArray[j].v[0], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[1], sizeof(int), 1, fout);
            fwrite(&gd->faceArray[j].v[2], sizeof(int), 1, fout);
            LOGI("face %d : %d, %d, %d\n", j, gd->faceArray[j].v[0], gd->faceArray[j].v[1], gd->faceArray[j].v[2]);
            SE_Vector3f p0, p1, p2, v1, v2;
            p0.x = gd->vertexArray[gd->faceArray[j].v[0]].x;
            p0.y = gd->vertexArray[gd->faceArray[j].v[0]].y;
            p0.z = gd->vertexArray[gd->faceArray[j].v[0]].z;
            
            p1.x = gd->vertexArray[gd->faceArray[j].v[1]].x;
            p1.y = gd->vertexArray[gd->faceArray[j].v[1]].y;
            p1.z = gd->vertexArray[gd->faceArray[j].v[1]].z;
            
            p2.x = gd->vertexArray[gd->faceArray[j].v[2]].x;
            p2.y = gd->vertexArray[gd->faceArray[j].v[2]].y;
            p2.z = gd->vertexArray[gd->faceArray[j].v[2]].z;
            /*
            SE_Vec3f_Subtract(&p1, &p0, &v1);
            SE_Vec3f_Subtract(&p2, &p0, &v2);
            float a = SE_Vec3f_CrossScalar(&v1, &v2);
            if(a < 0)
            {
                static int kk = 0;
                LOGI("### clock wise face: %d ##\n", kk++);
            }
            else if(a > 0)
            {
                LOGI("## counter clock wise face ##\n");
            }
             */
        }
        if(gd->texVertexNum > 0)
        {
            for(j = 0 ; j < gd->texVertexNum ; j++)
            {
                fwrite(&gd->texVertexArray[j].x, sizeof(float), 1, fout);
                fwrite(&gd->texVertexArray[j].y, sizeof(float), 1, fout);
                LOGI("tex vertex %d : %f, %f\n", j, gd->texVertexArray[j].x, gd->texVertexArray[j].y);
            }
            for(j = 0 ; j < gd->faceNum ; j++)
            {
                fwrite(&gd->texFaceArray[j].v[0], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[1], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray[j].v[2], sizeof(int), 1, fout);
                LOGI("tex face %d : %d, %d, %d\n", j, gd->texFaceArray[j].v[0], gd->texFaceArray[j].v[1], gd->texFaceArray[j].v[2]);
            }
        }
        if(gd->texVertexNum2 > 0)
        {
            for(j = 0 ; j < gd->texVertexNum2 ; j++)
            {
                fwrite(&gd->texVertexArray2[j].x, sizeof(float), 1, fout);
                fwrite(&gd->texVertexArray2[j].y, sizeof(float), 1, fout);
                LOGI("tex vertex %d : %f, %f\n", j, gd->texVertexArray2[j].x, gd->texVertexArray2[j].y);
            }
            for(j = 0 ; j < gd->faceNum ; j++)
            {
                fwrite(&gd->texFaceArray2[j].v[0], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray2[j].v[1], sizeof(int), 1, fout);
                fwrite(&gd->texFaceArray2[j].v[2], sizeof(int), 1, fout);
                LOGI("tex face %d : %d, %d, %d\n", j, gd->texFaceArray2[j].v[0], gd->texFaceArray2[j].v[1], gd->texFaceArray2[j].v[2]);
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
        fwrite(&gd->numMapChannel, sizeof(int), 1, fout);
        if(gd->numMapChannel > 0)
        {
            for(int k = 0 ; k < gd->numMapChannel ; k++)
            {
                SE_MapChannel* mc = &gd->mapChannelArray[k];
                fwrite(&mc->numTVertex, sizeof(int), 1, fout);
                fwrite(&mc->numTFaces, sizeof(int), 1, fout);
                for(int n = 0 ; n < mc->numTVertex ; n++)
                {
                    fwrite(&mc->texVertexArray[n].x, sizeof(float), 1 ,fout);
                    fwrite(&mc->texVertexArray[n].y, sizeof(float), 1, fout);
                }
                for(int n = 0 ; n < mc->numTFaces ; n++)
                {
                    fwrite(&mc->texFaceArray[n].v0, sizeof(int), 1, fout);
                    fwrite(&mc->texFaceArray[n].v1, sizeof(int), 1, fout);
                    fwrite(&mc->texFaceArray[n].v2, sizeof(int), 1, fout);
                }
            }
        }

        for(int i = 0 ; i < vertexBuffer.vertexNum ; i++)
        {
            SE_Vertex_XYZUV xyzuv = vertexBuffer.vertexArray[i];
            fwrite(&xyzuv.x, sizeof(float), 1, fout);
            fwrite(&xyzuv.y, sizeof(float), 1, fout);
            fwrite(&xyzuv.z, sizeof(float), 1, fout);
            fwrite(&xyzuv.u, sizeof(float), 1, fout);
            fwrite(&xyzuv.v, sizeof(float), 1, fout);
        }
        for(int i = 0 ; i < vertexBuffer.faceNum * 3; i++)
        {
            fwrite(&vertexBuffer.faceArray[i], sizeof(int), 1, fout);
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
        fwrite(&smesh->wireframeColor.x, sizeof(float), 1, fout);
        fwrite(&smesh->wireframeColor.y, sizeof(float), 1, fout);
        fwrite(&smesh->wireframeColor.z, sizeof(float), 1, fout);

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
            LOGI("...submesh num = %d\n", smesh->subMeshNum);
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

    for(i = 0 ; i < materialNum ; i++)
    {
        if(materials[i].subMaterialNum > 0)
        {
            for(int j = 0 ; j < materials[i].subMaterialNum ; j++)
	        {
		        SE_String_Release(&materials[i].subMaterialArray[j].texturename);
            }
             SE_Free(materials[i].subMaterialArray);
        }
	    SE_String_Release(&materials[i].materialData.texturename);
    } 
    SE_Free(materials);
    for(i = 0 ; i < geomDataNum ; i++)
    {
	    SE_Free(geomDataArray[i].vertexArray);
	    SE_Free(geomDataArray[i].texVertexArray);
	    SE_Free(geomDataArray[i].faceArray);
	    SE_Free(geomDataArray[i].texFaceArray);
	    SE_Free(geomDataArray[i].colorArray);
        SE_Free(geomDataArray[i].texVertexArray2);
        SE_Free(geomDataArray[i].texFaceArray2);
    }
    SE_Free(geomDataArray);
    for(itMesh = seMeshs.begin() ; itMesh != seMeshs.end() ; itMesh++)
    {
	SE_Mesh* mesh = *itMesh;
	if(mesh->subMeshArray)
	{
	    for(int j = 0 ; j < mesh->subMeshNum; j++)
	    { 
	        SE_SubMesh* subMesh = &mesh->subMeshArray[j];
		SE_Free(subMesh->faceList.faces);
	    }
	    SE_Free(mesh->subMeshArray);
	}
	SE_String_Release(&mesh->name);
	SE_Free(*itMesh);
    }
    //// write shader //
    fwrite(&SHADER_ID, sizeof(short), 1, fout);
    size_t shaderCount = mShaderList.size();
    fwrite(&shaderCount, sizeof(int), 1, fout);
    std::list<ASE_Shader*>::iterator itShader;
    for(itShader = mShaderList.begin() ; itShader != mShaderList.end() ; itShader++)
    {
        ASE_Shader* shader = *itShader;
        writeString(shader->shaderid.c_str(), fout);
        char* outData;
        int len;
        SE_ReadCScriptFile(shader->vertexShaderName.c_str(), &outData, &len);
        writeString(outData, len, fout);
        delete[] outData;
        len = 0;
        SE_ReadCScriptFile(shader->fragmentShaderName.c_str(), &outData, &len);
        writeString(outData, len, fout);
        delete[] outData;
        len = 0;
    }
    // write reference box;
    LOGI("... write reference box id \n");
    fwrite(&REFERENCE_BOX_ID, sizeof(short), 1, fout);
    fwrite(&mReferenceBox.min.x, sizeof(float), 1, fout);
    fwrite(&mReferenceBox.min.y, sizeof(float), 1, fout);
    fwrite(&mReferenceBox.min.z, sizeof(float), 1, fout);
    fwrite(&mReferenceBox.max.x, sizeof(float), 1, fout);
    fwrite(&mReferenceBox.max.y, sizeof(float), 1, fout);
    fwrite(&mReferenceBox.max.z, sizeof(float), 1, fout);
    //////////
    // write track point
    writeTrackPointList(TRACK_POINT_ID, mTrackData, fout);
    writeTrackPointList(VERTICAL_TRACK_POINT_ID, mVerticalTrackData, fout);
    /*
    LOGI("## track point dimension = %d , %d, %d\n", mTrackData.xlen, mTrackData.ylen, mTrackData.zlen);
    fwrite(&TRACK_POINT_ID, sizeof(short), 1, fout);
    fwrite(&mTrackData.xlen, sizeof(int), 1, fout);
    fwrite(&mTrackData.ylen, sizeof(int), 1, fout);
    fwrite(&mTrackData.zlen, sizeof(int), 1, fout);
    size_t trackPointListSize = mTrackData.trackPointsList.size();
    fwrite(&trackPointListSize, sizeof(int), 1, fout);
    std::list<ASE_TrackPointList>::iterator itTrackPointList;
    for(itTrackPointList = mTrackData.trackPointsList.begin() ; 
        itTrackPointList != mTrackData.trackPointsList.end() ;
        itTrackPointList++)
    {
        std::string name = itTrackPointList->name;
        writeString(name.c_str(), fout);
        LOGI("## track point list name = %s ##\n", name.c_str());
        std::list<ASE_TrackPoint>::iterator itTrackPoint;
        size_t trackPointCount = itTrackPointList->points.size();
        fwrite(&trackPointCount, sizeof(int), 1, fout);
        for(itTrackPoint = itTrackPointList->points.begin() ;
            itTrackPoint != itTrackPointList->points.end() ;
            itTrackPoint++)
        {
            LOGI("## track point = %f, %f, %f ##\n", itTrackPoint->x, itTrackPoint->y, itTrackPoint->z);
            fwrite(&itTrackPoint->x, sizeof(float), 1, fout);
            fwrite(&itTrackPoint->y, sizeof(float), 1, fout);
            fwrite(&itTrackPoint->z, sizeof(float), 1, fout);
        }
    }
    //write vertical track point
    LOGI("... vertical track point dimension = %d , %d, %d\n", mVerticalTrackData.xlen, mVerticalTrackData.ylen, mVerticalTrackData.zlen);
    fwrite(&VERTICAL_TRACK_POINT_ID, sizeof(short), 1, fout);
    fwrite(&mVerticalTrackData.xlen, sizeof(int), 1, fout);
    fwrite(&mVerticalTrackData.ylen, sizeof(int), 1, fout);
    fwrite(&mVerticalTrackData.zlen, sizeof(int), 1, fout);
    size_t verticalTrackPointListSize = mVerticalTrackData.trackPointsList.size();
    fwrite(&verticalTrackPointListSize, sizeof(int), 1, fout);
    std::list<ASE_TrackPointList>::iterator itVerticalTrackPointList;
    for(itVerticalTrackPointList = mVerticalTrackData.trackPointsList.begin() ; 
        itVerticalTrackPointList != mVerticalTrackData.trackPointsList.end() ;
        itVerticalTrackPointList++)
    {
        std::string name = itVerticalTrackPointList->name;
        writeString(name.c_str(), fout);
        LOGI("## track point list name = %s ##\n", name.c_str());
        std::list<ASE_TrackPoint>::iterator itTrackPoint;
        size_t trackPointCount = itVerticalTrackPointList->points.size();
        fwrite(&trackPointCount, sizeof(int), 1, fout);
        for(itTrackPoint = itVerticalTrackPointList->points.begin() ;
            itTrackPoint != itVerticalTrackPointList->points.end() ;
            itTrackPoint++)
        {
            LOGI("## track point = %f, %f, %f ##\n", itTrackPoint->x, itTrackPoint->y, itTrackPoint->z);
            fwrite(&itTrackPoint->x, sizeof(float), 1, fout);
            fwrite(&itTrackPoint->y, sizeof(float), 1, fout);
            fwrite(&itTrackPoint->z, sizeof(float), 1, fout);
        }
    }
     */
    //write looking point
    fwrite(&LOOKING_POINT_ID, sizeof(short), 1, fout);
    size_t lookingPointCount = mLookingPointList.size();
    fwrite(&lookingPointCount, sizeof(int), 1, fout);
    LookingPointList::iterator itLookingPoint;
    for(itLookingPoint = mLookingPointList.begin();
        itLookingPoint != mLookingPointList.end();
        itLookingPoint++)
    {
        writeString(itLookingPoint->name.c_str(), fout);
        fwrite(&itLookingPoint->point.x, sizeof(float), 1, fout);
        fwrite(&itLookingPoint->point.y, sizeof(float), 1, fout);
        fwrite(&itLookingPoint->point.z, sizeof(float), 1, fout);
    }
    //end
    //write looking point track
    size_t lookingPointTrackSize = mLookingPointTrackList.size();
    fwrite(&LOOKING_POINT_TRACK_ID, sizeof(short), 1, fout);
    fwrite(&lookingPointTrackSize, sizeof(int), 1, fout);
    std::list<ASE_LookingPointTrackData>::iterator itLookingPointTrack;
    for(itLookingPointTrack = mLookingPointTrackList.begin() ;
        itLookingPointTrack != mLookingPointTrackList.end();
        itLookingPointTrack++)
    {
        writeString(itLookingPointTrack->name.c_str(), fout);
        LOGI("## lookingpoint track name = %s ####\n", itLookingPointTrack->name.c_str());
        size_t sss = itLookingPointTrack->lookpointtrackList.size();
        fwrite(&sss, sizeof(int), 1, fout);
        std::list<ASE_LookingPointTrack>::iterator itLP;
        for(itLP = itLookingPointTrack->lookpointtrackList.begin();
            itLP != itLookingPointTrack->lookpointtrackList.end();
            itLP++)
        {
            LOGI("## percent = %d, name = %s, side = %d, frameNum = %d\n", itLP->percent,
                 itLP->lookpointname.c_str(), itLP->side, itLP->frameNum);
            fwrite(&itLP->percent, sizeof(int), 1, fout);
            writeString(itLP->lookpointname.c_str(), fout);
            fwrite(&itLP->side, sizeof(int), 1, fout);
            fwrite(&itLP->frameNum, sizeof(int), 1, fout);
        }
    }
    //end
    LOGI("write end\n");
    fclose(fout); 
}
void ASE_Loader::writeTrackPointList(short trackID, ASE_TrackData& trackData, FILE* fout)
{
    LOGI("... track point dimension = %d , %d, %d\n", mTrackData.xlen, mTrackData.ylen, mTrackData.zlen);
    fwrite(&trackID, sizeof(short), 1, fout);
    fwrite(&trackData.xlen, sizeof(int), 1, fout);
    fwrite(&trackData.ylen, sizeof(int), 1, fout);
    fwrite(&trackData.zlen, sizeof(int), 1, fout);
    size_t trackPointListSize = trackData.trackPointsList.size();
    fwrite(&trackPointListSize, sizeof(int), 1, fout);
    std::list<ASE_TrackPointList>::iterator itTrackPointList;
    for(itTrackPointList = trackData.trackPointsList.begin() ; 
        itTrackPointList != trackData.trackPointsList.end() ;
        itTrackPointList++)
    {
        std::string name = itTrackPointList->name;
        writeString(name.c_str(), fout);
        LOGI("## track point list name = %s ##\n", name.c_str());
        int adjustTrackListSize = TRACK_PHOTO_TYPE_NUM;
        fwrite(&adjustTrackListSize, sizeof(int), 1, fout);
        for(int i = 0 ; i < TRACK_PHOTO_TYPE_NUM ; i++)
        {
            ASE_AdjustTrackPointList& adjustTrackPointList = itTrackPointList->points[i];
            LOGI("## adjust point = %f, %f, %f ##\n", adjustTrackPointList.adjustx, adjustTrackPointList.adjusty, adjustTrackPointList.adjustz);
            fwrite(&adjustTrackPointList.adjustx, sizeof(float), 1, fout);
            fwrite(&adjustTrackPointList.adjusty, sizeof(float), 1 , fout);
            fwrite(&adjustTrackPointList.adjustz, sizeof(float), 1, fout);
            size_t trackPointListCount = adjustTrackPointList.trackList.size();
            fwrite(&trackPointListCount, sizeof(int), 1, fout);
            std::list<ASE_TrackPoint>::iterator itTrackPoint;
            for(itTrackPoint = adjustTrackPointList.trackList.begin() ;
                itTrackPoint != adjustTrackPointList.trackList.end() ;
                itTrackPoint++)
            {
                LOGI("## track point = %f, %f, %f ##\n", itTrackPoint->x, itTrackPoint->y, itTrackPoint->z);
                fwrite(&itTrackPoint->x, sizeof(float), 1, fout);
                fwrite(&itTrackPoint->y, sizeof(float), 1, fout);
                fwrite(&itTrackPoint->z, sizeof(float), 1, fout);
            }
        }
    }
}

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
    delete[] ase.buffer;
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
void ASE_Loader::ASE_KeySHADER(const char* token)
{
    /*
    if(!strcmp(token, "*NUM"))                              
    {           
        ASE_GetToken(false);                            
        int num = atoi(s_token);
        LOGI("...shader num = %d \n", num);      
    } 
     */
    if(!strcmp(token, "*SHADER"))                      
    {                                                       
        ASE_Shader* shader = new ASE_Shader;
        ASE_GetToken(false);                            
        shader->shaderid = s_token;                     
        ASE_GetToken(false);                            
        shader->vertexShaderName = s_token;             
        ASE_GetToken(false);                            
        shader->fragmentShaderName = s_token;           
        mShaderList.push_back(shader);
    }                 
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
        if(mCurrentMapChannel)
        {
            mCurrentMapChannel->tfaces[index].vi[0] = a;
            mCurrentMapChannel->tfaces[index].vi[1] = b;
            mCurrentMapChannel->tfaces[index].vi[2] = c;
        }
        else
        {
            if(mGeomObjComplement)
            {
                pMesh->tfaces2[index].vi[0] = a;
		        pMesh->tfaces2[index].vi[1] = b;
		        pMesh->tfaces2[index].vi[2] = c;
            }
            else
            {
		        pMesh->tfaces[index].vi[0] = a;
		        pMesh->tfaces[index].vi[1] = b;
		        pMesh->tfaces[index].vi[2] = c;
            }
        }

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
        if(mCurrentMapChannel != NULL)
        {
            mCurrentMapChannel->tvertexes[index].s = atof(u);
            mCurrentMapChannel->tvertexes[index].t = atof(v);
        }
        else
        {
            if(mGeomObjComplement)
            {
                pMesh->tvertexes2[index].s = atof(u);
                pMesh->tvertexes2[index].t = atof(v);
            }
            else
            {
		        pMesh->tvertexes[index].s = atof( u );
		        pMesh->tvertexes[index].t = atof( v );
            }
        }
	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_TVERTLIST" );
	}
}
void ASE_Loader::ASE_KeyMESHNormal(const char* token)
{
    ASE_Mesh *pMesh = mCurrMesh;
	if ( !strcmp( token, "*MESH_FACENORMAL" ) )
	{
        ASE_GetToken( false );
        int index = atoi(s_token);
		ASE_GetToken( false );
		float x = atof(token);
        
		ASE_GetToken( false );
		float y = atof(token);
        
		ASE_GetToken( false );
		float z = atof(token);
        ASE_Vertex v;
        v.x = x;
        v.y = y;
        v.z = z;
        pMesh->normalList.push_back(v);
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
        if(mGeomObjComplement)
        {
            pMesh->numTVertexes2 = atoi(s_token);
            LOGI( ".....num tvertexes2: %d\n", pMesh->numTVertexes2 );
        }
        else
        {
		    pMesh->numTVertexes = atoi( s_token );
            LOGI( ".....num tvertexes: %d\n", pMesh->numTVertexes );
        }
		
	}
	else if ( !strcmp( token, "*MESH_VERTEX_LIST" ) )
	{
		pMesh->vertexes = new ASE_Vertex[pMesh->numVertexes];
		LOGI(  ".....parsing MESH_VERTEX_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_VERTEX_LIST );
	}
	else if ( !strcmp( token, "*MESH_TVERTLIST" ) )
	{
        if(mGeomObjComplement)
        {
            pMesh->tvertexes2 = new ASE_TVertex[pMesh->numTVertexes2];
            LOGI(  ".....parsing MESH_TVERTLIST\n"  );
        }
        else
        {
		    pMesh->tvertexes = new ASE_TVertex[pMesh->numTVertexes];
		    LOGI(  ".....parsing MESH_TVERTLIST\n"  );
        }
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
        if(mGeomObjComplement)
        {
            pMesh->tfaces2 = new ASE_Face[pMesh->numFaces];
            LOGI(  ".....parsing MESH_TFACE_LIST\n"  );
        }
        else
        {
		    pMesh->tfaces = new ASE_Face[pMesh->numFaces];
		    LOGI(  ".....parsing MESH_TFACE_LIST\n"  );
        }
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyTFACE_LIST );
	}
	else if ( !strcmp( token, "*MESH_NORMALS" ) )
	{
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESHNormal );
	}
    else if(!strcmp(token, "*MESH_MAPPINGCHANNEL"))
    {
        mCurrentMapChannel = new ASE_MeshMapChannel;
        pMesh->mapChannels.push_back(mCurrentMapChannel);
        ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_MAPPINGCHENNEL);
        mCurrentMapChannel = NULL;
    }
}
void ASE_Loader::ASE_KeyMESH_MAPPINGCHENNEL(const char* token)
{
    ASE_Mesh* pMesh = mCurrMesh;
    if ( !strcmp( token, "*MESH_TVERTLIST" ) )
	{
		
		LOGI(  ".....parsing map channel MESH_TVERTLIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_TVERTLIST );
	}
    else if ( !strcmp( token, "*MESH_TFACELIST" ) )
	{
        LOGI(".... parsing map channel MESH_TFACELIST\n");
        ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyTFACE_LIST );
    }
    else if ( !strcmp( token, "*MESH_NUMTVFACES" ) )
	{
		ASE_GetToken( false );
		if ( atoi( s_token ) != pMesh->numFaces )
		{
			LOGE( "MESH_NUMTVFACES != MESH_NUMFACES" );
		}
        else
        {
            mCurrentMapChannel->numTFace = pMesh->numFaces;
            mCurrentMapChannel->tfaces = new ASE_Face[mCurrentMapChannel->numTFace];
            LOGI(".... map channel num face: %d\n", mCurrentMapChannel->numTFace);
        }
	}
	else if ( !strcmp( token, "*MESH_NUMTVERTEX" ) )
	{
		ASE_GetToken( false );
        mCurrentMapChannel->numTVertex = atoi(s_token);
        mCurrentMapChannel->tvertexes  = new ASE_TVertex[mCurrentMapChannel->numTVertex];
		LOGI( "..... map channel num tvertexes: %d\n", mCurrentMapChannel->numTVertex);
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
static std::string getTokenString()
{
    char name[512];
    memset(name, 0, 512);
    strcpy(name, s_token + 1 );
    if ( strchr( name, '"' ) )
        *strchr( name, '"' ) = 0;
    std::string str = name;
    return str;
}
void ASE_Loader::ASE_KeyGEOMOBJECT( const char *token )
{
	if ( !strcmp( token, "*NODE_NAME" ) )
	{
		ASE_GetToken( true );
		LOGI(  " %s\n", s_token  );
        if(mCurrGeomObject)
        {
            strcpy( mCurrGeomObject->name, s_token + 1 );
            if ( strchr( mCurrGeomObject->name, '"' ) )
                *strchr( mCurrGeomObject->name, '"' ) = 0;
            if(!strcmp(mCurrGeomObject->name , "Camera01"))
            {
                LOGI("... has camera setting\n");
            }
        }
        else
        {
            char name[512];
            memset(name, 0, 512);
            strcpy(name, s_token + 1 );
            if ( strchr( name, '"' ) )
                *strchr( name, '"' ) = 0;
            std::string str = name;
            GeomObjectList::iterator it;
            for(it = mSceneObject->mGeomObjects.begin() ; it != mSceneObject->mGeomObjects.end(); it++)
            {
                ASE_GeometryObject* obj = *it;
                if(str == obj->name)
                {
                    mCurrGeomObject = obj;
                    break;
                }
            }
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
    else if(!strcmp(token, "*MESH_UV2"))
    {
        mCurrMesh = mCurrGeomObject->mesh;
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
void ASE_Loader::putLookingPoint(const std::string& name, ASE_Vertex p)
{
    /*
    LookingPointList::iterator it;
    for(it = mLookingPointList.begin();
        it != mLookingPointList.end();
        it++)
    {
        if(it->name == name)
            it->point = p;
    }
     */
    ASE_LookingPoint lp;
    lp.name = name;
    lp.point = p;
    mLookingPointList.push_back(lp);
}

void ASE_Loader::ASE_KeyHELPEROBJECT(const char* token)
{
    if ( !strcmp( token, "*NODE_NAME" ) )
    {
        ASE_GetToken( true );
        mCurrLookingPoint = getTokenString();
    }
    else if(!strcmp(token , "*BOUNDINGBOX_MIN"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        putLookingPoint(mCurrLookingPoint, ASE_Vertex(x, y , z));
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
        else if(!strcmp(s_token , "*GEOMOBJECT_COMPLEMENT"))
        {
            LOGI("GEOMOBJECT_COMPLEMENT\n");
            mCurrGeomObject = NULL;
            mGeomObjComplement = true;
            ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyGEOMOBJECT );
            mGeomObjComplement = false;
        }
        else if(!strcmp(s_token, "*GEOMOBJECT_REFERENCE"))
        {
            LOGI("GEOMOBJECT_REFERENCE\n");
            mReferenceBox.reference = new ASE_GeometryObject;
            mCurrGeomObject = mReferenceBox.reference;
            ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyGEOMOBJECT );
        }
        else if(!strcmp(s_token, "*SHADERINFO"))
        {
            ASE_ParseBracedBlock(&ASE_Loader::ASE_KeySHADER);
        }
        else if(!strcmp(s_token, "*HELPEROBJECT"))
        {
            ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyHELPEROBJECT);
        }
	}
#ifdef DEBUG
	LOGI(".. geomCount = %d \n", geomCount);
#endif
    
}
void ASE_Loader::end()
{
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
void ASE_Loader::LoadLines(const char* filename)
{
	FILE *fp = fopen( filename, "rb" );
    
	if ( !fp )
    {
		LOGI("File not found '%s'", filename);
        return;
    }
	size_t len = getFileLen( fp );
    
	char* buffer = new char[len];
    
	LOGI( "Processing '%s'\n", filename );
    
	if ( fread(buffer, len, 1, fp ) != 1 )
	{
		fclose( fp );
		LOGI( "fread() != -1 for '%s'", filename );
        return;
	}
	fclose( fp );
	ASE_ProcessLine(buffer, len);
    delete[] buffer;    
}
void ASE_Loader::LoadVerticalTrackPoint(const char* filename)
{
    mCurrentLineType = TRACK_LIST_POINT;
    mCurrentTrackListType = VERTICAL;
    LoadLines(filename);
}
void ASE_Loader::LoadTrackPoint(const char* filename)
{
    mCurrentLineType = TRACK_LIST_POINT;
    mCurrentTrackListType = HORZ;
    LoadLines(filename);
}
void ASE_Loader::LoadLookPointTrack(const char* filename)
{
    mCurrentLineType = LOOKING_POINT_TRACK;
    LoadLines(filename);
}
bool ASE_Loader::isNum(const char* str)
{
    while(*str != '\0')
    {
        if(isnumber(*str) || (*str) == '.')
            str++;
        else
            return false;
    }
    return true;
}
#define TRACK_NAME 0
#define TRACK_POINT 1
#define TRACK_DIM 2
#define TRACK_PHOTO_TYPE 3
bool ASE_Loader::isTrackKeyWords(const std::string& str)
{
    std::map<std::string, TRACK_LIST_FOR_PHOTO_TYPE>::iterator it = mTrackListPhotoTypeMap.find(str);
    if(it == mTrackListPhotoTypeMap.end())
    {
        return false;
    }
    else 
    {
        return true;
    }
}
void ASE_Loader::handleLine(size_t line, std::vector<std::string>& tokenes)
{
    if(mCurrentLineType == TRACK_LIST_POINT)
    {
        if(line == 0)
        {
            handleTokenes(tokenes, TRACK_DIM);
        }
        else
        {
            if(tokenes[0][0] != '#')
            {
                if(isNum(tokenes[0].c_str()))
                {
                    handleTokenes(tokenes, TRACK_POINT);
                }
                else if(isTrackKeyWords(tokenes[0]))
                {
                    handleTokenes(tokenes, TRACK_PHOTO_TYPE);
                }
                else
                {
                    handleTokenes(tokenes, TRACK_NAME);
                }
            }
        }
    }
    else if(mCurrentLineType == LOOKING_POINT_TRACK)
    {
        if(isNum(tokenes[0].c_str()))
        {
            int column = 2;
            size_t n = tokenes.size() / column;
            size_t itOffset = 0;
            for(int i = 0 ; i < n ; i++)
            {
                std::list<ASE_LookingPointTrackData>::iterator it = se_list_nref(mLookingPointTrackList, mCurrentStartLookTrack + itOffset);
                std::string percent = tokenes[column * i];
                std::string lookpoint = tokenes[column * i + 1];
                std::vector<std::string> stringV = splitString(lookpoint.c_str(), "_");
                size_t stringVSize = stringV.size() ;
                assert(stringV.size() > 2);
                if(stringV[1] == "start")
                {
                    int s;
                    s = 1;
                    LOGI("## start point ###\n");
                }
                if(stringV[1] == "frame" || stringV[1] == "start")
                {
                    std::string lookpointname = stringV[0];
                    for(int i = 1 ; i < stringV.size() - 2 ; i++)
                    {
                        lookpointname += "_";
                        lookpointname += stringV[i];
                    }
                    std::string sideStr = stringV[stringVSize - 2];
                    std::string frameNumStr = stringV[stringVSize - 1];
                    ASE_LookingPointTrack lpt;
                    lpt.percent = atoi(percent.c_str());
                    lpt.lookpointname = lookpointname;
                    lpt.side = sideStr == "left" ? 0 : 1;
                    lpt.frameNum = atoi(frameNumStr.c_str());
                    it->lookpointtrackList.push_back(lpt);
                }
                else if(stringV[1] == "model")
                {
                    std::string lookpointname = lookpoint;
                    ASE_LookingPointTrack lpt;
                    lpt.percent = atoi(percent.c_str());
                    lpt.lookpointname = lookpointname;
                    lpt.side = 2;
                    lpt.frameNum = 1;
                    it->lookpointtrackList.push_back(lpt);
                }
                else
                {
                    assert(0);
                }
                
                itOffset++;
            }
        }
        else
        {
            mCurrentStartLookTrack = mLookingPointTrackList.size();
            for(int i = 0 ; i < tokenes.size() ; i++)
            {
                std::string name = tokenes[i];
                ASE_LookingPointTrackData trackdata;
                trackdata.name = name;
                mLookingPointTrackList.push_back(trackdata);
            }
        }
    }
}
void ASE_Loader::ASE_ProcessLine(char* buffer, size_t len)
{
    size_t line = 0;
    size_t index = 0;
    const size_t LINE_SIZE = 512;
    while(index < len)
    {
        char buf[LINE_SIZE];
        memset(buf, 0, LINE_SIZE);
        bool ret = getLine(buf, LINE_SIZE, index, buffer, len);
        if(!ret)
        {
            break;
        }
        std::vector<std::string> tokenes = getTokens(buf, strlen(buf));
        if(tokenes.size() > 0)
        {
            LOGI("line = %s\n", buf);
            handleLine(line, tokenes);
        }
        line++;
    }
    if(index < len)
    {
        LOGI("parse line %d error\n", line);
        return;
    }    
}
static bool isWhiteSpace(char c)
{
    return c == ' ' || c == '\r' || c == '\t';
}
std::vector<std::string> ASE_Loader::getTokens(char* buffer, size_t size)
{
    size_t index = 0;
    size_t start = 0;
    std::list<std::string> ret;
    while(index < size)
    {
        while((index < size) && isWhiteSpace(buffer[index]))
        {
            index++;
        }
        if(index >= size)
            break;
        start = index;
        while((index < size) && !isWhiteSpace(buffer[index]))
            index++;
        std::string str(buffer + start, index - start);
        ret.push_back(str);
    }
    std::vector<std::string> strV;
    strV.resize(ret.size());
    if(ret.size() > 0)
        std::copy(ret.begin(), ret.end(), strV.begin());
    return strV;
}
bool ASE_Loader::getLine(char *line, int lineLen, size_t& index, char* data, size_t dataLen)
{
    memset(line, 0 , lineLen);
    size_t currPos = index;
    while(index < dataLen && data[index] != '\n')
        index++;
    if(index >= dataLen)
        return false;
    size_t currLineLen = index - currPos;
    if(currLineLen > 0)
    {
        if(currLineLen >= lineLen)
        {
            LOGI("## read line exceed the max size of buffer ##\n");
            return false;
        }
        for(size_t i = 0 ; i < currLineLen ; i++)
            line[i] = data[currPos + i];
    }
    index++;
    return true;
}
void ASE_Loader::putTrackAdjust(const std::string& name, float x, float y , float z)
{
    std::list<ASE_TrackPointList>::iterator it;
    if(mCurrentTrackListType == HORZ)
    {
        for(it = mTrackData.trackPointsList.begin() ; it != mTrackData.trackPointsList.end() ; it++)
        {
            if(it->name == name)
            {
                it->points[mCurrentTrackListPhotoType].adjustx = x;
                it->points[mCurrentTrackListPhotoType].adjusty = y;
                it->points[mCurrentTrackListPhotoType].adjustz = z;
            }
        }
    }
    else 
    {
        for(it = mVerticalTrackData.trackPointsList.begin() ; it != mVerticalTrackData.trackPointsList.end() ; it++)
        {
            if(it->name == name)
            {
                it->points[mCurrentTrackListPhotoType].adjustx = x;
                it->points[mCurrentTrackListPhotoType].adjusty = y;
                it->points[mCurrentTrackListPhotoType].adjustz = z;
            }
        }
    }

}
void ASE_Loader::handleTokenes(std::vector<std::string>& tokenes, int status)
{
    switch (status) 
    {
        case TRACK_NAME:
        {
            mCurrTrackName.resize(tokenes.size());
            for(size_t i = 0 ; i < mCurrTrackName.size() ; i++)
            {
                ASE_TrackPointList p;
                p.name = tokenes[i];
                mCurrTrackName[i] = tokenes[i];
                if(mCurrentTrackListType == HORZ)
                {
                    mTrackData.trackPointsList.push_back(p);
                }
                else 
                {
                    mVerticalTrackData.trackPointsList.push_back(p);
                }
            }
        }
        break;
        case TRACK_PHOTO_TYPE:
        {
            int size = tokenes.size();
            assert(size  == mCurrTrackName.size() * 3 + 1);
            std::string trackPhotoType = tokenes[0];
            assert(isTrackKeyWords(trackPhotoType));
            mCurrentTrackListPhotoType = mTrackListPhotoTypeMap[trackPhotoType];
            for(size_t i = 0 ; i < mCurrTrackName.size() ; i++)
            {
                float x = atof(tokenes[3 * i + 0 + 1].c_str());
                float y = atof(tokenes[3 * i + 1 + 1].c_str());
                float z = atof(tokenes[3 * i + 2 + 1].c_str());
                putTrackAdjust(mCurrTrackName[i], x, y, z);
            }
            
        }
            break;
        case TRACK_DIM:
        {
            SE_ASSERT(tokenes.size() == 3);
            if(mCurrentTrackListType == HORZ)
            {
                mTrackData.xlen = atoi(tokenes[0].c_str());
                mTrackData.ylen = atoi(tokenes[1].c_str());
                mTrackData.zlen = atoi(tokenes[2].c_str());
            }
            else
            {
                mVerticalTrackData.xlen = atoi(tokenes[0].c_str());
                mVerticalTrackData.ylen = atoi(tokenes[1].c_str());
                mVerticalTrackData.zlen = atoi(tokenes[2].c_str());     
            }
        }
        break;
        case TRACK_POINT:
        {
            SE_ASSERT(tokenes.size() == 3 * mCurrTrackName.size());
            for(size_t i = 0 ; i < mCurrTrackName.size() ; i++)
            {
                float x = atof(tokenes[3 * i + 0].c_str());
                float y = atof(tokenes[3 * i + 1].c_str());
                float z = atof(tokenes[3 * i + 2].c_str());
                putTrackPoint(mCurrTrackName[i], ASE_TrackPoint(x, y, z));
            }
        }
        break;
        default:
            break;
    }
}
void ASE_Loader::putTrackPoint(const std::string& name, ASE_TrackPoint p)
{
    std::list<ASE_TrackPointList>::iterator it;
    if(mCurrentTrackListType == HORZ)
    {
        for(it = mTrackData.trackPointsList.begin() ; it != mTrackData.trackPointsList.end() ; it++)
        {
            if(it->name == name)
            {
                it->points[mCurrentTrackListPhotoType].trackList.push_back(p);
            }
        }
    }
    else 
    {
        for(it = mVerticalTrackData.trackPointsList.begin() ; it != mVerticalTrackData.trackPointsList.end() ; it++)
        {
            if(it->name == name)
            {
                it->points[mCurrentTrackListPhotoType].trackList.push_back(p);
            }
        }
    }
}
///////////////////////////////
