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
#include "SS_OpenGL.h"
#include "SEShaderDefine.h"
//////////
SE_Texture::~SE_Texture()
{
    GLuint t[1];
    t[0] = texture;
    glDeleteTextures(1, t);
}
///////////////////////////
void SE_Mesh::removeGLBuffer()
{
    if(vaoID > 0)
    {
        glDeleteVertexArraysOES(1, &vaoID);
        vaoID = 0;
    }
    for(int i = 0 ; i < 2 ; i++)
    {
        for(int j = 0 ; j < 4 ; j++)
        {
            glDeleteVertexArraysOES(1, &vaoIDArray[i][j].vao);
            glDeleteBuffers(1, &vaoIDArray[i][j].uvVbo);
        }
    }
    for(int i = 0 ;i  < 2 ; i++)
    {
        for(int j = 0 ;j < 4 ; j++)
        {
            glDeleteVertexArraysOES(1, &mirrorVaoIDArray[i][j].vao);
            glDeleteBuffers(1, &mirrorVaoIDArray[i][j].uvVbo);
        }
    }
    if(vboID > 0)
    {
        glDeleteBuffers(1, &vboID);
        vboID = 0;
    }
    if(indexVboID > 0)
    {
        glDeleteBuffers(1, &indexVboID);
        indexVboID = 0;
    }
    for(int i = 0 ; i < 2 ; i++)
    {
        for(int j = 0 ; j < 4 ; j++)
        {
            vaoIDArray[i][j].vao = 0;
            vaoIDArray[i][j].uvVbo = 0;
        }
    }
    for(int i = 0 ; i < 2 ; i++)
    {
        for(int j = 0 ; j < 4 ; j++)
        {
            mirrorVaoIDArray[i][j].vao = 0;
            mirrorVaoIDArray[i][j].uvVbo = 0;
        }
    }
    //glFinish();
}
SE_AABB SE_Mesh::getAABB()
{
    SS_ModelManager* mm = SS_GetModelManager();
    SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
    return gd->getAABB();
}
unsigned short* SE_Mesh::getIndexBuffer(VERTEX_TYPE type)
{
    if(type == XYZ_UV)
    {
        SS_ModelManager* mm = SS_GetModelManager();
        SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
        return gd->vertexBufferFaceArray;
    }else {
        return NULL;
    }
}
int SE_Mesh::getIndexBufferNum(VERTEX_TYPE type)
{
    if(type == XYZ_UV)
    {
        SS_ModelManager* mm = SS_GetModelManager();
        SE_GeometryData* gd = mm->getGeometryData(geomDataIndex);
        return gd->faceNum * 3;
    }
    else {
        return NULL;
    }
}
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
        if(mCurrVertexType != INVALID)
            SE_ASSERT(0);
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
            assert(faceNum == gd->texFaceNum);
            /*
            int vertexNum = faceNum * 3;
            int floatSize = vertexNum * 5;
            //_VertexXYZ_UV* data = new _VertexXYZ_UV[vertexNum];
            float* data = new float[floatSize];
            //i = 0;
            int j = 0;
            for(int i = 0; i < faceNum ; i++)
            //for(itFace = visibileFaceList.begin() ; itFace != visibileFaceList.end() ; itFace++)
            {
                SE_Face f = gd->faceArray[i];
                //SE_Face f = itFace->f;
                //SE_Face fuv = itFace->fuv;
                SE_Face fuv = gd->texFaceArray[i];
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
             */
            mDrawingVertex = (float*)gd->vertexBufferArray;
            mDrawingVertexNum = gd->vertexBufferNum;
            mCurrVertexType = type;
            mFloatSize = mDrawingVertexNum * 5;
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
static const short LOOKING_POINT_TRACK_ID = 0x0011;
static const short VERTICAL_TRACK_POINT_ID = 0x0012;
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
void SS_ModelManager::readTrackPointData(SE_TrackPointData& trackPointData, const char* data, int& startPos)
{
    trackPointData.xlen = readInt(data, &startPos);
    trackPointData.ylen = readInt(data, &startPos);
    trackPointData.zlen = readInt(data, &startPos);
    int trackPointListCount = readInt(data, &startPos);
    LOGI("... track point dimension = %d, %d, %d\n", trackPointData.xlen, trackPointData.ylen, trackPointData.zlen);
    trackPointData.trackPointList.resize(trackPointListCount);
    LOGI("... track point list num = %d\n", trackPointListCount);
    for(int i = 0 ; i < trackPointListCount ; i++)
    {
        trackPointData.trackPointList[i].name = readString(data, &startPos);
        LOGI("... track point list name = %s\n", trackPointData.trackPointList[i].name.c_str());
        int adjustTrackPointCount = readInt(data, &startPos);
        assert(adjustTrackPointCount == 4);
        for(int j = 0 ; j < adjustTrackPointCount ; j++)
        {
            trackPointData.trackPointList[i].points[j].adjustx = readFloat(data, &startPos);
            trackPointData.trackPointList[i].points[j].adjusty = readFloat(data, &startPos);
            trackPointData.trackPointList[i].points[j].adjustz = readFloat(data, &startPos);
            LOGI("... adjust point = %f, %f, %f\n", trackPointData.trackPointList[i].points[j].adjustx, trackPointData.trackPointList[i].points[j].adjusty ,
                 trackPointData.trackPointList[i].points[j].adjustz);
            int trackPointCount = readInt(data, &startPos);
            LOGI("... track point count = %d\n", trackPointCount);
            trackPointData.trackPointList[i].points[j].trackList.resize(trackPointCount);
            for(int k = 0 ; k < trackPointCount ; k++)
            {
                
                trackPointData.trackPointList[i].points[j].trackList[k].x = readFloat(data, &startPos);
                trackPointData.trackPointList[i].points[j].trackList[k].y = readFloat(data, &startPos);
                trackPointData.trackPointList[i].points[j].trackList[k].z = readFloat(data, &startPos);
                LOGI("... track point = %f, %f, %f\n", trackPointData.trackPointList[i].points[j].trackList[k].x, trackPointData.trackPointList[i].points[j].trackList[k].y,
                     trackPointData.trackPointList[i].points[j].trackList[k].z);
            }
        }
    }

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
            int vertexBufferNum = readInt(data, &startPos);
            int vertexBufferFaceNum = readInt(data, &startPos);
            
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
            LOGI("... vertex buffer num = %d \n", vertexBufferNum);
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
            SE_Vertex_XYZUV* vertexBuffer = new SE_Vertex_XYZUV[vertexBufferNum];
            unsigned short* vertexBufferFace = new unsigned short[faceNum * 3];
            for(int i = 0 ; i < vertexBufferNum ; i++)
            {
                vertexBuffer[i].x = readFloat(data, &startPos);
                vertexBuffer[i].y = readFloat(data, &startPos);
                vertexBuffer[i].z = readFloat(data, &startPos);
                vertexBuffer[i].u = readFloat(data, &startPos);
                vertexBuffer[i].v = readFloat(data, &startPos);
            }
            for(int i = 0 ; i < faceNum * 3  ; i++)
            {
                vertexBufferFace[i] = (unsigned short)readInt(data, &startPos);
            }
            gd->setVertexes(vertexArray, vertexNum, SE_GeometryData::SE_NOCOPY);
            gd->setTexVertexes(texVertexArray, texVertexNum, SE_GeometryData::SE_NOCOPY);
            gd->setFaces(faceArray, faceNum, SE_GeometryData::SE_NOCOPY);
            gd->setTexFaces(texFaceArray, faceNum , SE_GeometryData::SE_NOCOPY);
            gd->setColors(colorArray, colorNum, SE_GeometryData::SE_NOCOPY);
            gd->vertexBufferNum = vertexBufferNum;
            gd->vertexBufferArray = vertexBuffer;
            gd->vertexBufferFaceArray = vertexBufferFace;
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
            //mShaderArray.resize(shaderNum);
            for(int i = 0 ; i < shaderNum ; i++)
            {
                std::string shaderid = readString(data, &startPos);
                char* outDataVertex = NULL;
                int lenVertex = 0;
                readCharArray(data, &startPos, &outDataVertex, &lenVertex);
                char* outDataFragment = NULL;
                int lenFragment = 0;
                readCharArray(data, &startPos, &outDataFragment, &lenFragment);
                delete[] outDataVertex;
                delete[] outDataFragment;
                //SS_Shader* s = new SS_Shader(shaderid.c_str(), outDataVertex, outDataFragment);
                //mShaderArray[i] = s;
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
            readTrackPointData(mTrackPointData, data, startPos);
        }
        else if(currChunckId == VERTICAL_TRACK_POINT_ID)
        {
            readTrackPointData(mVerticalTrackPointData, data, startPos);
            /*
            mVerticalTrackPointData.xlen = readInt(data, &startPos);
            mVerticalTrackPointData.ylen = readInt(data, &startPos);
            mVerticalTrackPointData.zlen = readInt(data, &startPos);
            int trackPointListCount = readInt(data, &startPos);
            mVerticalTrackPointData.trackPointList.resize(trackPointListCount);
            for(int i = 0 ; i < trackPointListCount ; i++)
            {
                mVerticalTrackPointData.trackPointList[i].name = readString(data, &startPos);
                int trackPointCount = readInt(data, &startPos);
                mVerticalTrackPointData.trackPointList[i].points.resize(trackPointCount);
                for(int j = 0 ; j < trackPointCount ; j++)
                {
                    mVerticalTrackPointData.trackPointList[i].points[j].x = readFloat(data, &startPos);
                    mVerticalTrackPointData.trackPointList[i].points[j].y = readFloat(data, &startPos);
                    mVerticalTrackPointData.trackPointList[i].points[j].z = readFloat(data, &startPos);
                }
            }
             */
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
        else if(currChunckId == LOOKING_POINT_TRACK_ID)
        {
            int lookingPointTrackDataNum = readInt(data, &startPos);
            LOGI("## looking point track num = %d ##\n", lookingPointTrackDataNum);
            mLookingPointTrackList.resize(lookingPointTrackDataNum);
            for(int i = 0 ; i < lookingPointTrackDataNum; i++)
            {
                std::string name = readString(data, &startPos);
                mLookingPointTrackList[i].name = name;
                int size = readInt(data, &startPos);
                mLookingPointTrackList[i].lookingPointTrackDataList.resize(size);
                for(int j = 0 ; j < size; j++)
                {
                    int percent = readInt(data, &startPos);
                    std::string lookpointname = readString(data, &startPos);
                    int side = readInt(data, &startPos);
                    int frameNum = readInt(data, &startPos);
                    mLookingPointTrackList[i].lookingPointTrackDataList[j].percent = percent;
                    mLookingPointTrackList[i].lookingPointTrackDataList[j].side = side;
                    mLookingPointTrackList[i].lookingPointTrackDataList[j].lookpointname = lookpointname;
                    mLookingPointTrackList[i].lookingPointTrackDataList[j].frameNum = frameNum;
                }
            }
        }
    }
    SEShaderDefine* shaderDefine = new SEShaderDefine;
    std::vector<std::string> allShaderId = shaderDefine->getAllShaderID();
    mShaderArray.resize(allShaderId.size());
    for(int i = 0 ; i < allShaderId.size() ; i++)
    {
        std::string shaderid = allShaderId[i];
        SEShaderDefine::ShaderSrcData shaderData = shaderDefine->getShaderSrc(shaderid);
        SS_Shader* s = new SS_Shader(shaderid.c_str(), shaderData.vertexShaderSrc, shaderData.fragmentShaderSrc);
        mShaderArray[i] = s;
    }
    delete shaderDefine;
    SE_ASSERT(startPos == dataLen);
}

//////////////////////////
SS_ModelManager::SS_ModelManager()
{}
SS_ModelManager::~SS_ModelManager()
{
    GeometryDataArray::iterator itGeomData;
    for(itGeomData = mGeometryDataArray.begin() ; itGeomData != mGeometryDataArray.end(); itGeomData++)
    {
        itGeomData->release();
    }
    
    MaterialDataArray::iterator itMaterial;
    for(itMaterial = mMaterialArray.begin() ; itMaterial != mMaterialArray.end() ; itMaterial++)
    {
        delete[] itMaterial->subMaterialArray;
    }
    for(int i = 0 ; i < mShaderArray.size() ; i++)
    {
        delete mShaderArray[i];
    }
}
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
int SS_ModelManager::getFullTextureCount()
{
    TextureList::iterator it;
    int count = 0;
    LOGI("########################\n");
    for(it = mFullImageTextureList.begin() ; it != mFullImageTextureList.end() ; it++)
    {
        std::string name = it->first;
        //std::size_t pos = name.find("_full");
        //if(pos != std::string::npos)
        {
            LOGI("## full texture = %s , %p ##\n", name.c_str(), it->second);
            count++;
        }
    }
    LOGI("##########################\n");
    LOGI("## full image texture count = %d ##\n", count);
    return count;
}
SE_Texture* SS_ModelManager::getTexture(TextureList& textureList, const char* textureName)
{
    TextureList::iterator it;
    for(it = textureList.begin() ; it != textureList.end() ; it++)
    {
        if(it->first == textureName)
            return it->second;
    }
    return  NULL;
}
void SS_ModelManager::setTexture(TextureList& textureList, const char* textureName, SE_Texture* texture)
{
    for(TextureList::iterator it = textureList.begin() ; it != textureList.end() ; it++)
    {
        if(it->first == textureName)
        {
            delete it->second;
            it->second = texture;
            return;
        }
    }
    textureList[textureName] = texture;
}
void SS_ModelManager::removeTexture(TextureList& textureList, const char* textureName)
{
    TextureList::iterator it = textureList.find(textureName);
    if(it != textureList.end())
    {
        delete it->second;
        LOGI("## remove texture %s #\n", textureName);
        textureList.erase(it);
    }
}
SE_Texture* SS_ModelManager::SS_ModelManager::getFullImageTexture(const char* textureName)
{
    //return mFullImageTextureList[textureName];
    return getTexture(mFullImageTextureList, textureName);
}
void SS_ModelManager::setFullImageTexture(const char* textureName, SE_Texture* t)
{
    setTexture(mFullImageTextureList, textureName, t);
}
void SS_ModelManager::removeFullImageTexture(const char* textureName)
{
    removeTexture(mFullImageTextureList, textureName);
}
SE_Texture* SS_ModelManager::getTexture(const char* texturename)
{
    //return mTextureList[texturename];
    return getTexture(mTextureList, texturename);
}
int SS_ModelManager::getTextureCount() const
{
    return mTextureList.size();
}
void SS_ModelManager::setTexture(const char* texturename, SE_Texture* texture)
{
    /*
    assert(texture != NULL);
    SE_Texture* t = mTextureList[texturename];
    if(t)
        delete t;
    mTextureList[texturename] = texture;
     */
    setTexture(mTextureList, texturename, texture);
}
void SS_ModelManager::removeTexture(const char* texturename)
{
    /*
    TextureList::iterator it = mTextureList.find(texturename);
    if(it != mTextureList.end())
    {
        delete it->second;
        LOGI("## remove texture %s #\n", texturename);
        mTextureList.erase(it);
    }
     */
    removeTexture(mTextureList, texturename);
}
void SS_ModelManager::removeAllTexture()
{
    TextureList::iterator it;
    for(it = mTextureList.begin() ; it != mTextureList.end(); it++)
    {
        std::string name = it->first;
        LOGI("## texture name = %s ##\n", name.c_str());
        delete  it->second;
    }
    mTextureList.clear();
    for(it = mFullImageTextureList.begin() ; it != mFullImageTextureList.end() ; it++)
    {
        std::string name = it->first;
        LOGI("full image texture name = %s \n", name.c_str());
        delete it->second;
    }
    mFullImageTextureList.clear();
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
void SS_ModelManager::removeAllShaderFromGL()
{
    for(size_t i = 0 ; i < mShaderArray.size() ; i++)
    {
        SS_Shader* s = mShaderArray[i];
        s->removeFromGL();
    }
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
std::vector<std::string> SS_ModelManager::getAllTrackPointsName(VIEW_TYPE viewType)
{
    if(viewType == LANDSCAPE)
    {
        std::vector<std::string> ret(mTrackPointData.trackPointList.size());
        std::vector<SE_TrackPointList>::iterator it;
        int i = 0;
        for(it = mTrackPointData.trackPointList.begin() ; it != mTrackPointData.trackPointList.end();it++)
        {
            ret[i++] = it->name;
        }
        return ret;
    }
    else
    {
        std::vector<std::string> ret(mVerticalTrackPointData.trackPointList.size());
        std::vector<SE_TrackPointList>::iterator it;
        int i = 0;
        for(it = mVerticalTrackPointData.trackPointList.begin() ; it != mVerticalTrackPointData.trackPointList.end();it++)
        {
            ret[i++] = it->name;
        }
        return ret;
    }
}
SE_TrackPoint SS_ModelManager::getTrackPointAdjust(VIEW_TYPE viewType, TRACK_LIST_FOR_PHOTO_TYPE trackPhotoType, const char* name)
{
    if(viewType == LANDSCAPE)
    {
        std::vector<SE_TrackPointList>::iterator it;
        SE_TrackPoint ret;
        for(it = mTrackPointData.trackPointList.begin();
            it != mTrackPointData.trackPointList.end();
            it++)
        {
            if(it->name == name)
            {
                ret.x = it->points[trackPhotoType].adjustx;
                ret.y = it->points[trackPhotoType].adjusty;
                ret.z = it->points[trackPhotoType].adjustz;
                return ret;
            }
        }
        return ret;
    }
    else {

        std::vector<SE_TrackPointList>::iterator it;
        SE_TrackPoint ret;
        for(it = mVerticalTrackPointData.trackPointList.begin();
            it != mVerticalTrackPointData.trackPointList.end();
            it++)
        {
            if(it->name == name)
            {

                ret.x = it->points[trackPhotoType].adjustx;
                ret.y = it->points[trackPhotoType].adjusty;
                ret.z = it->points[trackPhotoType].adjustz;
                return ret;
            }
        }
        return ret;
    }

}
std::vector<SE_TrackPoint> SS_ModelManager::getTrackPoints(VIEW_TYPE viewType, TRACK_LIST_FOR_PHOTO_TYPE trackPhotoType, const char* name)
{
    if(viewType == LANDSCAPE)
    {
        std::vector<SE_TrackPoint> ret;
        std::vector<SE_TrackPointList>::iterator it;
        for(it = mTrackPointData.trackPointList.begin();
            it != mTrackPointData.trackPointList.end();
            it++)
        {
            if(it->name == name)
                return it->points[trackPhotoType].trackList;
        }
        return ret;
    }
    else {
        std::vector<SE_TrackPoint> ret;
        std::vector<SE_TrackPointList>::iterator it;
        for(it = mVerticalTrackPointData.trackPointList.begin();
            it != mVerticalTrackPointData.trackPointList.end();
            it++)
        {
            if(it->name == name)
                return it->points[trackPhotoType].trackList;
        }
        return ret;
    }
}
SE_Vector3f SS_ModelManager::getTrackPoint(VIEW_TYPE viewType, const SE_TrackPoint& tp, const SE_Vector3f& start)
{
    return getTrackPoint(viewType, tp.x, tp.y, tp.z, start);
}
SE_Vector3f SS_ModelManager::getTrackPoint(VIEW_TYPE viewType, float x, float y, float z, const SE_Vector3f& start)
{
    if(viewType == LANDSCAPE)
    {
        float xspan = mReferenceBoxMax.x - mReferenceBoxMin.x;
        float yspan = mReferenceBoxMax.y - mReferenceBoxMin.y;
        float zspan = mReferenceBoxMax.z - mReferenceBoxMin.z;
        float xstep = xspan / mTrackPointData.xlen;
        float ystep = yspan / mTrackPointData.ylen;
        float zstep = zspan / mTrackPointData.zlen;
        //SE_Vector3f start(mReferenceBoxMin.x, mReferenceBoxMax.y, mReferenceBoxMax.z);
        //return SE_Vector3f(start.x + (x - 1) * xstep + 0.5 * xstep, start.y - 0.5 * ystep - (y - 1) * ystep, start.z - 0.5 * zstep - (z - 1) * zstep);
        return SE_Vector3f(start.x + (x - 1) * xstep, start.y - y * ystep, start.z - z * zstep);
    }
    else
    {
        float xspan = mReferenceBoxMax.x - mReferenceBoxMin.x;
        float yspan = mReferenceBoxMax.y - mReferenceBoxMin.y;
        float zspan = mReferenceBoxMax.z - mReferenceBoxMin.z;
        float xstep = xspan / mVerticalTrackPointData.xlen;
        float ystep = yspan / mVerticalTrackPointData.ylen;
        float zstep = zspan / mVerticalTrackPointData.zlen;
        //SE_Vector3f start(mReferenceBoxMin.x, mReferenceBoxMax.y, mReferenceBoxMax.z);
        //return SE_Vector3f(start.x + (x - 1) * xstep + 0.5 * xstep, start.y - 0.5 * ystep - (y - 1) * ystep, start.z - 0.5 * zstep - (z - 1) * zstep);
        return SE_Vector3f(start.x + (x - 1) * xstep, start.y - y * ystep, start.z - z * zstep);
    }
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
SE_LookingPointTrackData SS_ModelManager::getLookingPointTrackData(const std::string& name, int index)
{
    for(int i = 0 ; i < mLookingPointTrackList.size() ; i++)
    {
        if(mLookingPointTrackList[i].name == name)
        {
            return mLookingPointTrackList[i].lookingPointTrackDataList[index];
        }
    }
    return SE_LookingPointTrackData();
}
std::vector<SE_LookingPointTrackData> SS_ModelManager::getLookingPointTrackDataList(const std::string& name)
{
    std::vector<SE_LookingPointTrackData> ret;
    for(int i = 0 ; i < mLookingPointTrackList.size() ; i++)
    {
        if(mLookingPointTrackList[i].name == name)
            return mLookingPointTrackList[i].lookingPointTrackDataList;
    }
    return ret;
}
int SS_ModelManager::getLookingPointTrackDataNum(const std::string& name)
{
    for(int i = 0 ; i < mLookingPointTrackList.size() ; i++)
    {
        if(mLookingPointTrackList[i].name == name)
        {
            return mLookingPointTrackList[i].lookingPointTrackDataList.size();
        }
    }
    return 0;
}