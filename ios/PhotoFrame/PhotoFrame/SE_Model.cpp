//
//  SE_Model.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "SS_Model.h"
#include "SE_Log.h"
#include "PGMDataReader.h"
#include "SS_Shader.h"
//////////
SE_Texture::~SE_Texture()
{
    
}
///////////////////////////
float* SE_Mesh::getWireframeVertex(const SE_Vector3f& cameraZ)
{
    if(mWireFrameVertex)
        return mWireFrameVertex;
    SS_ModelManager* mm = SS_GetModelManager();
    SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
    int faceNum = gd->faceNum;
    std::list<SE_Face> visibleFaceList;
    for(int i = 0 ; i < faceNum ; i++)
    {
        SE_Face f = gd->faceArray[i];
        SE_Vector3f v0 = gd->vertexArray[f.v0];
        SE_Vector3f v1 = gd->vertexArray[f.v1];
        SE_Vector3f v2 = gd->vertexArray[f.v2];
        SE_Vector3f d0 = v1 - v0;
        SE_Vector3f d1 = v2 - v0;
        SE_Vector3f n = d0.cross(d1);
        if(n.dot(cameraZ) > 0)
        {
            visibleFaceList.push_back(f);
        }
    }
    faceNum = visibleFaceList.size();
    int edgeNum = faceNum * 3;
    int vertexNum = edgeNum * 2;
    int floatSize = vertexNum * 3;
    float* vertexBuffer = new float[floatSize];
    int j = 0;
    std::list<SE_Face>::iterator itFace;
    for(itFace = visibleFaceList.begin() ; itFace != visibleFaceList.end() ; itFace++)
    {
        SE_Face f = *itFace;
        SE_Vector3f v0 = gd->vertexArray[f.v0];
        SE_Vector3f v1 = gd->vertexArray[f.v1];
        SE_Vector3f v2 = gd->vertexArray[f.v2];
        vertexBuffer[j++] = v0.x;
        vertexBuffer[j++] = v0.y;
        vertexBuffer[j++] = v0.z;
        
        vertexBuffer[j++] = v1.x;
        vertexBuffer[j++] = v1.y;
        vertexBuffer[j++] = v1.z;
        
        vertexBuffer[j++] = v1.x;
        vertexBuffer[j++] = v1.y;
        vertexBuffer[j++] = v1.z;
        
        vertexBuffer[j++] = v2.x;
        vertexBuffer[j++] = v2.y;
        vertexBuffer[j++] = v2.z;
        
        vertexBuffer[j++] = v2.x;
        vertexBuffer[j++] = v2.y;
        vertexBuffer[j++] = v2.z;
        
        vertexBuffer[j++] = v0.x;
        vertexBuffer[j++] = v0.y;
        vertexBuffer[j++] = v0.z;
    }
    mWireFrameVertex = vertexBuffer;
    mWireFrameVertexNum = vertexNum;
    SE_ASSERT(j == floatSize);
    return mWireFrameVertex;
}
SE_VertexProperty SE_Mesh::getVertexProperty(VERTEX_TYPE vt)
{
    SE_VertexProperty vp;
    switch (vt) 
    {
        case XYZ:
            vp.stride = SE_VertexProperty::XYZ_SIZE * sizeof(float);
            vp.xyzOffset = 0;
            break;
        case XYZ_UV:
            vp.stride = (SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE) * sizeof(float);
            vp.xyzOffset = 0;
            vp.uv1Offset = SE_VertexProperty::XYZ_SIZE;
            break;
        case XYZ_UV1_UV2:
            vp.stride = (SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE * 2) * sizeof(float);
            vp.xyzOffset = 0;
            vp.uv1Offset = SE_VertexProperty::XYZ_SIZE;
            vp.uv2Offset = SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE;
            break;
        case XYZ_COLOR:
            vp.stride = (SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::COLOR_SIZE) * sizeof(float);
            vp.xyzOffset = 0;
            vp.colorOffset = SE_VertexProperty::XYZ_SIZE;
            break;
        case XYZ_UV_NORMAL:
            vp.stride = (SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE + SE_VertexProperty::NORMAL_SIZE) * sizeof(float);
            vp.xyzOffset = 0;
            vp.uv1Offset = SE_VertexProperty::XYZ_SIZE;
            vp.normalOffset = SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE;
            break;
        default:
            break;
    }
    return vp;
}
struct FaceData
{
    SE_Face f;
    SE_Face fuv;
};
float* SE_Mesh::getDrawingVertex(VERTEX_TYPE type)
{
    if(mCurrVertexType != type)
    {
        delete[] mDrawingVertex;
        mDrawingVertex = NULL;
    }
    if(mDrawingVertex)
        return mDrawingVertex;
    switch (type) 
    {
        case XYZ_UV:
        {
            SS_ModelManager* mm = SS_GetModelManager();
            SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
            if(!gd)
                return NULL;
            if(!gd->texVertexArray)
                return NULL;
            int faceNum = gd->faceNum;
            int i = 0;
            std::list<FaceData> visibileFaceList;
            //debug
            for(i = 0 ; i < faceNum ; i++)
            {
                SE_Face f = gd->faceArray[i];
                SE_Face fuv = gd->texFaceArray[i];
                SE_Vector3f v0 = gd->vertexArray[f.v0];
                SE_Vector3f v1 = gd->vertexArray[f.v1];
                SE_Vector3f v2 = gd->vertexArray[f.v2];
                //if(v0.x <= -8.9801 || v1.x <= -8.9801 || v2.x <= -8.9801)
                {
                    FaceData fd;
                    fd.f = f;
                    fd.fuv = fuv;
                    visibileFaceList.push_back(fd);
                }
            }
            faceNum = visibileFaceList.size();
            std::list<FaceData>::iterator itFace;
            //end
            int vertexNum = faceNum * 3;
            int floatSize = vertexNum * 5;
            //_VertexXYZ_UV* data = new _VertexXYZ_UV[vertexNum];
            float* data = new float[floatSize];
            i = 0;
            int j = 0;
            //for(int i = 0; i < faceNum ; i++)
            for(itFace = visibileFaceList.begin() ; itFace != visibileFaceList.end() ; itFace++)
            {
                //SE_Face f = gd->faceArray[i];
                SE_Face f = itFace->f;
                SE_Face fuv = itFace->fuv;
                //fuv = gd->texFaceArray[i];
                //i++;
                data[j++] = gd->vertexArray[f.v0].x;
                data[j++] = gd->vertexArray[f.v0].y;
                data[j++] = gd->vertexArray[f.v0].z;
                data[j++] = gd->texVertexArray[fuv.v0].x;
                data[j++] = gd->texVertexArray[fuv.v0].y;
                
                data[j++] = gd->vertexArray[f.v1].x;
                data[j++] = gd->vertexArray[f.v1].y;
                data[j++] = gd->vertexArray[f.v1].z;
                data[j++] = gd->texVertexArray[fuv.v1].x;
                data[j++] = gd->texVertexArray[fuv.v1].y;
                
                data[j++] = gd->vertexArray[f.v2].x;
                data[j++] = gd->vertexArray[f.v2].y;
                data[j++] = gd->vertexArray[f.v2].z;
                data[j++] = gd->texVertexArray[fuv.v2].x;
                data[j++] = gd->texVertexArray[fuv.v2].y;
                                
            }
            SE_ASSERT(j == floatSize);
            mDrawingVertex = (float*)data;
            mDrawingVertexNum = vertexNum;
            mCurrVertexType = type;
            mFloatSize = floatSize;
            return mDrawingVertex;
        }
        break;
        case XYZ_COLOR:
        {
            SS_ModelManager* mm = SS_GetModelManager();
            SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
            if(!gd)
                return NULL;
            if(gd->colorArray == NULL)
                return  NULL;
            int faceNum =gd->faceNum;
            int vertexNum = faceNum * 3;
            int floatSize = vertexNum * 6;
            float* data = new float[floatSize];
            int j = 0;
            for(int i = 0 ; i < faceNum ; i++)
            {
                SE_Face f = gd->faceArray[i];
                data[j++] = gd->vertexArray[f.v0].x;
                data[j++] = gd->vertexArray[f.v0].y;
                data[j++] = gd->vertexArray[f.v0].z;
                data[j++] = gd->colorArray[f.v0].x;
                data[j++] = gd->colorArray[f.v0].y;
                data[j++] = gd->colorArray[f.v0].z;
                
                data[j++] = gd->vertexArray[f.v1].x;
                data[j++] = gd->vertexArray[f.v1].y;
                data[j++] = gd->vertexArray[f.v1].z;
                data[j++] = gd->colorArray[f.v1].x;
                data[j++] = gd->colorArray[f.v1].y;
                data[j++] = gd->colorArray[f.v1].z;
                
                data[j++] = gd->vertexArray[f.v2].x;
                data[j++] = gd->vertexArray[f.v2].y;
                data[j++] = gd->vertexArray[f.v2].z;
                data[j++] = gd->colorArray[f.v2].x;
                data[j++] = gd->colorArray[f.v2].y;
                data[j++] = gd->colorArray[f.v2].z;
            }
            SE_ASSERT(j == floatSize);
            mDrawingVertexNum = vertexNum;
            mCurrVertexType = type;
            mFloatSize = floatSize;
            mDrawingVertex = data;
            return data;
        }
        break;
        case XYZ_UV_NORMAL:
        {}
        break;
        case XYZ_UV1_UV2:
        {
            SS_ModelManager* mm = SS_GetModelManager();
            SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
            if(!gd)
                return NULL;
            if(gd->texVertexArray == NULL)
                return NULL;
            if(gd->texVertexArray2 == NULL)
                return NULL;
            int faceNum = gd->faceNum;
            int vertexNum = faceNum * 3;
            int floatSize = vertexNum * (SE_VertexProperty::XYZ_SIZE + SE_VertexProperty::UV_SIZE * 2);
            int j = 0;
            float* data = new float[floatSize];
            for(int i = 0 ; i < faceNum ; i++)
            {
                SE_Face f = gd->faceArray[i];
                SE_Face fuv1 = gd->texFaceArray[i];
                SE_Face fuv2 = gd->texFaceArray2[i];
                data[j++] = gd->vertexArray[f.v0].x;
                data[j++] = gd->vertexArray[f.v0].y;
                data[j++] = gd->vertexArray[f.v0].z;
                data[j++] = gd->texVertexArray[fuv1.v0].x;
                data[j++] = gd->texVertexArray[fuv1.v0].y;
                data[j++] = gd->texVertexArray2[fuv2.v0].x;
                data[j++] = gd->texVertexArray2[fuv2.v0].y;
                
                data[j++] = gd->vertexArray[f.v1].x;
                data[j++] = gd->vertexArray[f.v1].y;
                data[j++] = gd->vertexArray[f.v1].z;
                data[j++] = gd->texVertexArray[fuv1.v1].x;
                data[j++] = gd->texVertexArray[fuv1.v1].y;
                data[j++] = gd->texVertexArray2[fuv2.v1].x;
                data[j++] = gd->texVertexArray2[fuv2.v1].y;
                
                data[j++] = gd->vertexArray[f.v2].x;
                data[j++] = gd->vertexArray[f.v2].y;
                data[j++] = gd->vertexArray[f.v2].z;
                data[j++] = gd->texVertexArray[fuv1.v2].x;
                data[j++] = gd->texVertexArray[fuv1.v2].y;
                data[j++] = gd->texVertexArray2[fuv2.v2].x;
                data[j++] = gd->texVertexArray2[fuv2.v2].y;
                
            }
            SE_ASSERT(j == floatSize);
            mDrawingVertexNum = vertexNum;
            mCurrVertexType = type;
            mFloatSize = floatSize;
            mDrawingVertex = data;
            return data;
        }
        break;
        default:
            break;
    }
    return NULL;
}
///////////////////////////
/** function about load image and mesh */
static const short MATERIAL_ID = 0x0002;
static const short GEOMOBJECT_ID = 0x0003;
static const short CAMERA_ID = 0x0004;
static const short SUB_MATERIAL_ID = 0x0005;
static const short MESH_ID = 0x0006;
static const short SHADER_ID = 0x0007;
static const short REFERENCE_BOX_ID = 0x0008;
static const short TRACK_POINT_ID = 0x0009;
static const short LOOKING_POINT_ID = 0x0010;
static const int MAGIC = 0xCFCFCFCF;
static const int VERSION = 0x01;
static const int COORDINATE = 0x00;
static const int ENDIAN = 0x00; /** 0: little endian, 1: big endian*/

static int readInt(const char* data, int* currPos)
{
    int v;
    memcpy(&v, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    return v;
}
static float readFloat(const char* data, int* currPos)
{
    float v;
    memcpy(&v, data + (*currPos), sizeof(float));
    (*currPos) += sizeof(float);
    return v;
}
static short readShort(const char* data, int* currPos)
{
    short v;
    memcpy(&v, data + (*currPos), sizeof(short));
    (*currPos) += sizeof(short);
    return v;
}
static std::string readString(const char* data, int* currPos)
{
    int len;
	char* buf;
    memcpy(&len, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    buf = new char[len + 1];
    memset(buf, 0 , len + 1);
    if(len > 0)
        strncpy(buf, data + (*currPos), len);
    std::string str = buf;
    delete[] buf;
    (*currPos) += len;
    return str;
}
static void readCharArray(const char* data, int* currPos, char** outData, int* len)
{
    *len = readInt(data, currPos);
    *outData = new char[*len];
    memcpy(*outData, data + (*currPos), *len);
    (*currPos) += *len;
}
void SS_ModelManager::addGeometryData(const SE_GeometryData& geomData)
{
    std::vector<SE_GeometryData> newGeomData;
    newGeomData.resize(mGeometryDataArray.size() + 1);
    size_t i;
    for(i = 0 ; i  < mGeometryDataArray.size() ; i++)
    {
        newGeomData[i] = mGeometryDataArray[i];
    }
    newGeomData[i] = geomData;
    mGeometryDataArray = newGeomData;
}
void SS_ModelManager::process(const char* data, int currPos, int dataLen)
{
    int magic, version, coordinate, endian;
    int startPos = currPos;
	int materialNum, geomObjNum, meshNum;
	int currMat, currMesh, currGeomObj;
	short currChunckId;
    magic = readInt(data, &startPos);
    version = readInt(data, &startPos);
    coordinate = readInt(data, &startPos);
    endian = readInt(data, &startPos);
    if(magic != MAGIC)
        return;
    if(version != VERSION)
        return;
    materialNum = readInt(data, &startPos);
    geomObjNum = readInt(data, &startPos);
    meshNum = readInt(data, &startPos);
    LOGI("...material num = %d\n", materialNum);
    LOGI("...geom obj num = %d\n", geomObjNum);
    LOGI("... mesh num = %d\n", meshNum);
    mMaterialArray.resize(materialNum);
    mGeometryDataArray.resize(geomObjNum);
    mMeshArray.resize(meshNum);
    currMat = 0;
    currGeomObj = 0;
    currMesh = 0;
    while(startPos < dataLen)
    {
        currChunckId = readShort(data, &startPos);
        LOGI("...currChunckId = %x\n", currChunckId);
        if(currChunckId == MATERIAL_ID)
        {
            float x, y, z;
			int subMtlNum;
			SE_Material* m = &mMaterialArray[currMat++];
            LOGI("...handle material\n");
            m->materialData.texturename = readString(data, &startPos);
            LOGI("...texture name = %s\n", m->materialData.texturename.c_str());
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            m->materialData.ambient = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            m->materialData.diffuse = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            m->materialData.specular = SE_Vector3f(x, y, z);
            subMtlNum = readInt(data, &startPos);
            m->subMaterialNum = subMtlNum;
            if(subMtlNum > 0)
            {
                int i;
                m->subMaterialArray = new SE_MaterialData[subMtlNum];
                for(i = 0 ; i < subMtlNum ; i++)
                {
                    m->subMaterialArray[i].texturename = readString(data, &startPos);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    m->subMaterialArray[i].ambient = SE_Vector3f(x, y, z);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    m->subMaterialArray[i].diffuse = SE_Vector3f(x, y, z);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    m->subMaterialArray[i].specular = SE_Vector3f(x, y, z);
                    
                }    
            } 
        }
        else if(currChunckId == GEOMOBJECT_ID)
        {
            int type = readInt(data, &startPos);
            int vertexNum = readInt(data, &startPos);
            int faceNum = readInt(data, &startPos);
            int texVertexNum = readInt(data, &startPos);
            int texVertexNum2 = readInt(data, &startPos);
            int colorNum = readInt(data, &startPos);
            
			SE_Face* faceArray = NULL;
			SE_Vector3f* vertexArray = NULL;
            SE_Vector2f* texVertexArray = NULL;
            SE_Vector2f* texVertexArray2 = NULL;
            SE_Face* texFaceArray = NULL;
            SE_Face* texFaceArray2 = NULL;
            SE_Vector3f* colorArray = NULL;
            int i;
			LOGI("... read the geom obj : %d\n", currGeomObj);
            LOGI("... face num = %d\n", faceNum);
            LOGI("... vertex num = %d\n", vertexNum);
            LOGI("... tex vertex num = %d\n", texVertexNum);
            LOGI("... tex vertex2 num = %d\n", texVertexNum2);
            LOGI("... color num = %d\n", colorNum);
            SE_GeometryData* gd = &mGeometryDataArray[currGeomObj++];
            gd->type = type;
            faceArray = new SE_Face[faceNum];
            if(!faceArray)
            {
                LOGE("out of memory when alloc face\n");
            } 
            vertexArray = new SE_Vector3f[vertexNum];
            if(!vertexArray)
            {
                LOGE("out of memory when alloc vertex\n");
            }
            
            if(texVertexNum > 0)
            {
                texVertexArray = new SE_Vector2f[texVertexNum];
                if(!texVertexArray)
                {
                    LOGE("out of memory when alloc tex vertex ");
                }
            }
            if(texVertexNum2 > 0)
            {
                texVertexArray2 = new SE_Vector2f[texVertexNum2];
                if(!texVertexArray2)
                {
                    LOGE("out of memory when alloc tex vertex2 ");
                }
            }
            for(i = 0 ; i < vertexNum ; i++)
            {
                vertexArray[i].x = readFloat(data, &startPos);
                vertexArray[i].y = readFloat(data, &startPos);
                vertexArray[i].z = readFloat(data, &startPos);
            }
            for(i = 0 ; i < faceNum ; i++)
            {
                faceArray[i].v[0] = readInt(data, &startPos);
                faceArray[i].v[1] = readInt(data, &startPos);
                faceArray[i].v[2] = readInt(data, &startPos);
            }
            if(texVertexNum > 0)
            {
                for(i = 0 ; i < texVertexNum ; i++)
                {
                    texVertexArray[i].x = readFloat(data, &startPos);
                    texVertexArray[i].y = readFloat(data, &startPos);
                }
                texFaceArray = new SE_Face[faceNum];
                if(!texFaceArray)
                {
                    LOGE("out of memory when alloc tex face\n");
                }
                for(i = 0 ; i < faceNum ; i++)
                {
                    texFaceArray[i].v[0] = readInt(data, &startPos);
                    texFaceArray[i].v[1] = readInt(data, &startPos);
                    texFaceArray[i].v[2] = readInt(data, &startPos);
                    
                }
            }
            if(texVertexNum2 > 0)
            {
                for(i = 0 ; i < texVertexNum2 ; i++)
                {
                    texVertexArray2[i].x = readFloat(data, &startPos);
                    texVertexArray2[i].y = readFloat(data, &startPos);
                }
                texFaceArray2 = new SE_Face[faceNum];
                if(!texFaceArray2)
                {
                    LOGE("out of memory when alloc tex face\n");
                }
                for(i = 0 ; i < faceNum ; i++)
                {
                    texFaceArray2[i].v[0] = readInt(data, &startPos);
                    texFaceArray2[i].v[1] = readInt(data, &startPos);
                    texFaceArray2[i].v[2] = readInt(data, &startPos);
                    
                }
                gd->texVertexArray2 = texVertexArray2;
                gd->texVertexNum2 = texVertexNum2;
                gd->texFaceArray2 = texFaceArray2;
                gd->texFaceNum2 = faceNum;
            }
            if(colorNum > 0)
            {
                colorArray = new SE_Vector3f[colorNum];
                if(!colorArray)
                {
                    LOGE("out of memory when alloc color array\n");
                }
                for(i = 0 ; i < colorNum ; i++)
                {
                    colorArray[i].x = readFloat(data, &startPos);
                    colorArray[i].y = readFloat(data, &startPos);
                    colorArray[i].z = readFloat(data, &startPos);
                }
            }
            int mapChannelNum = readInt(data, &startPos);
            if(mapChannelNum > 0)
            {
                gd->mapChannelNum = mapChannelNum;
                gd->mapChannelArray = new SE_MapChannel[gd->mapChannelNum];
                for(i = 0 ; i < gd->mapChannelNum ; i++)
                {
                    gd->mapChannelArray[i].numTVertex = readInt(data, &startPos);
                    gd->mapChannelArray[i].numTFaces = readInt(data, &startPos);
                    gd->mapChannelArray[i].texVertexArray = new SE_Vector2f[gd->mapChannelArray[i].numTVertex];
                    gd->mapChannelArray[i].texFaceArray = new SE_Face[gd->mapChannelArray[i].numTFaces];
                    for(int j = 0 ; j < gd->mapChannelArray[i].numTVertex ; j++)
                    {
                        gd->mapChannelArray[i].texVertexArray[j].x = readFloat(data, &startPos);
                        gd->mapChannelArray[i].texVertexArray[j].y = readFloat(data, &startPos);
                    }
                    for(int j = 0 ; j < gd->mapChannelArray[i].numTFaces ; j++)
                    {
                        gd->mapChannelArray[i].texFaceArray[j].v0 = readInt(data, &startPos);
                        gd->mapChannelArray[i].texFaceArray[j].v1 = readInt(data, &startPos);
                        gd->mapChannelArray[i].texFaceArray[j].v2 = readInt(data, &startPos);
                    }
                }
            }
            gd->setVertexes(vertexArray, vertexNum, SE_GeometryData::SE_NOCOPY);
            gd->setTexVertexes(texVertexArray, texVertexNum, SE_GeometryData::SE_NOCOPY);
            gd->setFaces(faceArray, faceNum, SE_GeometryData::SE_NOCOPY);
            gd->setTexFaces(texFaceArray, faceNum , SE_GeometryData::SE_NOCOPY);
            gd->setColors(colorArray, colorNum, SE_GeometryData::SE_NOCOPY);
        }
        else if(currChunckId == MESH_ID)
        {
            float x, y,z;
			int subMeshNum;
            std::string meshName;
            LOGI("... handle mesh\n");
            LOGI("... current mesh = %d\n", currMesh);
            SE_Mesh* mesh = &mMeshArray[currMesh++];
			mesh->geomDataIndex = readInt(data, &startPos);
            mesh->materialIndex = readInt(data, &startPos);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            mesh->wireframeColor = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            mesh->rotateAxis = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            mesh->rotateAngle = x;
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            mesh->scaleAxis = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            mesh->scale = SE_Vector3f(x, y, z);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            mesh->translate = SE_Vector3f(x, y, z);
            meshName = readString(data, &startPos);
            LOGI("... mesh name = %s\n", meshName.c_str());
            mesh->name = meshName;
            subMeshNum = readInt(data, &startPos);
            mesh->subMeshNum = subMeshNum;
            LOGI("...subMeshNum = %d\n", subMeshNum);
            if(subMeshNum > 0)
            {
                int i;
                mesh->subMeshArray = new SE_SubMesh[subMeshNum];
                for(i = 0 ; i < subMeshNum ; i++)
                {
					int j;
					int fn;
					int* facets;
					SE_FaceList* faceList;
                    SE_SubMesh* subMesh = &mesh->subMeshArray[i];
                    subMesh->subMaterialIndex = readInt(data, &startPos);
                    fn = readInt(data, &startPos);
                    faceList = &subMesh->faceList;
                    facets = new int[fn];
                    if(!facets)
                    {
                        LOGE("out of memory when alloc face list\n");
                    }
                    for(j = 0 ; j < fn ; j++)
                    {
                        facets[j] = readInt(data, &startPos);
                    }
                    faceList->facets = facets;
                    faceList->num = fn;
                } 
            }
        }
        else if(currChunckId == SHADER_ID)
        {
            int shaderNum = readInt(data, &startPos);
            mShaderArray.resize(shaderNum);
            for(int i = 0 ; i < shaderNum ; i++)
            {
                std::string shaderid = readString(data, &startPos);
                char* outDataVertex = NULL;
                int lenVertex = 0;
                readCharArray(data, &startPos, &outDataVertex, &lenVertex);
                char* outDataFragment = NULL;
                int lenFragment = 0;
                readCharArray(data, &startPos, &outDataFragment, &lenFragment);
                SS_Shader* s = new SS_Shader(shaderid.c_str(), outDataVertex, outDataFragment);
                mShaderArray[i] = s;
            }
        }
        else if(currChunckId == REFERENCE_BOX_ID)
        {
            float x1, y1, z1, x2, y2, z2;
            x1 = readFloat(data, &startPos);
            y1 = readFloat(data, &startPos);
            z1 = readFloat(data, &startPos);
            x2 = readFloat(data, &startPos);
            y2 = readFloat(data, &startPos);
            z2 = readFloat(data, &startPos);
            mReferenceBoxMin = SE_Vector3f(x1, y1, z1);
            mReferenceBoxMax = SE_Vector3f(x2, y2, z2);
        }
        else if(currChunckId == TRACK_POINT_ID)
        {
            mTrackPointData.xlen = readInt(data, &startPos);
            mTrackPointData.ylen = readInt(data, &startPos);
            mTrackPointData.zlen = readInt(data, &startPos);
            int trackPointListCount = readInt(data, &startPos);
            mTrackPointData.trackPointList.resize(trackPointListCount);
            for(int i = 0 ; i < trackPointListCount ; i++)
            {
                mTrackPointData.trackPointList[i].name = readString(data, &startPos);
                int trackPointCount = readInt(data, &startPos);
                mTrackPointData.trackPointList[i].points.resize(trackPointCount);
                for(int j = 0 ; j < trackPointCount ; j++)
                {
                    mTrackPointData.trackPointList[i].points[j].x = readInt(data, &startPos);
                    mTrackPointData.trackPointList[i].points[j].y = readInt(data, &startPos);
                    mTrackPointData.trackPointList[i].points[j].z = readInt(data, &startPos);
                }
            }
        }
        else if(currChunckId == LOOKING_POINT_ID)
        {
            int lookingPointCount = readInt(data, &startPos);
            mLookingPointList.resize(lookingPointCount);
            for(int i = 0 ; i < lookingPointCount ; i++)
            {
                std::string name = readString(data, &startPos);
                float x = readFloat(data, &startPos);
                float y = readFloat(data, &startPos);
                float z = readFloat(data, &startPos);
                mLookingPointList.push_back(SE_LookingPoint(name, SE_Vector3f(x, y , z)));
            }
        }
    }
    
    SE_ASSERT(startPos == dataLen);
}

//////////////////////////
SS_ModelManager::SS_ModelManager()
{}
SS_ModelManager::~SS_ModelManager()
{}
void SS_ModelManager::loadModel(const char* filename)
{
    const char* data;
    int len;
    SS_GetPgmData(filename, &data, &len);
    process(data, 0, len);
}
SE_Mesh* SS_ModelManager::getMesh(const char* meshName)
{
    MeshArray::iterator it;
    size_t count = mMeshArray.size();
    for(size_t i = 0 ; i < count ; i++)
    {
        SE_Mesh* mesh = &mMeshArray[i];
        if(mesh->name == meshName)
            return mesh;
    }
    return NULL;
}

SE_Texture* SS_ModelManager::getTexture(const char* texturename)
{
    return mTextureList[texturename];
}
void SS_ModelManager::setTexture(const char* texturename, SE_Texture* texture)
{
    SE_Texture* t = mTextureList[texturename];
    if(t)
        delete t;
    mTextureList[texturename] = texture;
}
void SS_ModelManager::removeTexture(const char* texturename)
{
    TextureList::iterator it = mTextureList.find(texturename);
    if(it != mTextureList.end())
    {
        delete it->second;
        mTextureList.erase(it);
    }
    
}
int SS_ModelManager::getMeshNum()
{
    return mMeshArray.size();
}
SE_Mesh* SS_ModelManager::getMesh(int index)
{
    return &mMeshArray[index];
}
SS_Shader* SS_ModelManager::getShader(const char* shaderName)
{
    for(size_t i = 0 ; i < mShaderArray.size() ; i++)
    {
        SS_Shader* s = mShaderArray[i];
        if(s->name() == shaderName)
            return s;
    }
    return NULL;
}
SE_GeometryData* SS_ModelManager::getGeometryData(int index)
{
    return &mGeometryDataArray[index];
}
SE_Material* SS_ModelManager::getMaterial(int index)
{
    return &mMaterialArray[index];
}
size_t SS_ModelManager::getGeometryDataNum()
{
    return mGeometryDataArray.size();
}
std::vector<SE_TrackPoint> SS_ModelManager::getTrackPoints(const char* name)
{
    std::vector<SE_TrackPoint> ret;
    std::vector<SE_TrackPointList>::iterator it;
    for(it = mTrackPointData.trackPointList.begin();
        it != mTrackPointData.trackPointList.end();
        it++)
    {
        if(it->name == name)
            return it->points;
    }
    return ret;
}
SE_Vector3f SS_ModelManager::getTrackPoint(const SE_TrackPoint& tp, const SE_Vector3f& start)
{
    return getTrackPoint(tp.x, tp.y, tp.z, start);
}
SE_Vector3f SS_ModelManager::getTrackPoint(int x, int y, int z, const SE_Vector3f& start)
{
    float xspan = mReferenceBoxMax.x - mReferenceBoxMin.x;
    float yspan = mReferenceBoxMax.y - mReferenceBoxMin.y;
    float zspan = mReferenceBoxMax.z - mReferenceBoxMin.z;
    float xstep = xspan / mTrackPointData.xlen;
    float ystep = yspan / mTrackPointData.ylen;
    float zstep = zspan / mTrackPointData.zlen;
    //SE_Vector3f start(mReferenceBoxMin.x, mReferenceBoxMax.y, mReferenceBoxMax.z);
    return SE_Vector3f(start.x + (x - 1) * xstep + 0.5 * xstep, start.y - 0.5 * ystep - (y - 1) * ystep, start.z - 0.5 * zstep - (z - 1) * zstep);
}
SE_Vector3f SS_ModelManager::getFrameLookingPoint(const char* name)
{
    LookingPointList::iterator it;
    for(it = mLookingPointList.begin();
        it != mLookingPointList.end();
        it++)
    {
        if(it->name == name)
            return it->point;
    }
    return SE_Vector3f();
}