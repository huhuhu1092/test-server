#include "SE_GeometryData.h"
#define SE_ARRAY_COPY(type, srcArray, srcArrayNum, dstArray, dstArrayNum) do{ \
    (dstArray) = new type[srcArrayNum]; \
    if((dstArray)) \
    { \
	    int i; \
        (dstArrayNum) = (srcArrayNum); \
        for(i = 0 ; i < (srcArrayNum) ; i++) \
        { \
            (dstArray)[i] = (srcArray)[i]; \
        } \
    } \
}while(0)
SE_GeometryData::SE_GeometryData(SE_GEOM_TYPE type, SE_Vector3f* vertexArray, int vertexNum,
                                      SE_Vector2f* texVertexArray, int texVertexNum,
                                      SE_Face* faceArray, int faceNum,
                                      SE_Face* texFaceArray, int texFaceNum,
                                      SE_Vector3f* normalArray, int normalNum,
                                      SE_Vector3f* colorArray, int colorNum)
{
    this->type = type;
    if(vertexNum > 0 && vertexArray)
    {
        SE_ARRAY_COPY(SE_Vector3f, vertexArray, vertexNum, this->vertexArray, this->vertexNum);
    }
    else
    {
        this->vertexNum = 0;
        this->vertexArray = NULL;
    }
    if(texVertexArray && texVertexNum > 0)
    {
        SE_ARRAY_COPY(SE_Vector2f, texVertexArray, texVertexNum, this->texVertexArray, this->texVertexNum);
    }
    else
    {
        this->texVertexNum = 0;
        this->texVertexArray = NULL;
    }
    if(normalArray && normalNum > 0)
    {
        SE_ARRAY_COPY(SE_Vector3f, normalArray, normalNum, this->normalArray, this->normalNum);
    }
    else
    {
        this->normalArray = NULL;
        this->normalNum = 0;
    }
    if(faceArray && faceNum > 0)
    {
        SE_ARRAY_COPY(SE_Face, faceArray, faceNum, this->faceArray, this->faceNum);
    }
    else
    {
        this->faceNum = 0;
        this->faceArray = NULL;
    }
    
    if(texFaceArray && texFaceNum > 0)
    {
        SE_ARRAY_COPY(SE_Face, texFaceArray, texFaceNum, this->texFaceArray, this->texFaceNum);
    }
    else
    {
        this->texFaceArray = NULL;
        this->texFaceNum = 0;
    }
    if(colorArray && colorNum > 0)
    {
        SE_ARRAY_COPY(SE_Vector3f, colorArray, colorNum, this->colorArray, this->colorNum);
    }
    else
    {
        this->colorNum = 0;
        this->colorArray = NULL;
    }
}
void SE_GeometryData::release()
{
    delete[] vertexArray;
    delete[] texVertexArray;
    delete[] faceArray;
    delete[] normalArray;
    delete[] texFaceArray;
    delete[] colorArray;
    for(int i = 0 ; i < mapChannelNum ; i++)
    {
        delete[] mapChannelArray[i].texVertexArray;
        delete[] mapChannelArray[i].texFaceArray;
    }
    delete[] mapChannelArray;
    
    delete[] vertexBufferArray;
    delete [] vertexBufferFaceArray;
    
    delete [] texVertexArray2;
    delete[] texFaceArray2;
    delete aabb;
    resetData();
}
SE_GeometryData::~SE_GeometryData()
{
}
void SE_GeometryData::clone(SE_GeometryData* outData)
{
    outData->setVertexes(vertexArray, vertexNum, SE_COPY);
    outData->setTexVertexes(texVertexArray, texVertexNum, SE_COPY);
    outData->setFaces(faceArray, faceNum, SE_COPY);
    outData->setTexFaces(texFaceArray, texFaceNum, SE_COPY);
    outData->setNormals(normalArray, normalNum, SE_COPY);
    outData->setColors(colorArray, colorNum, SE_COPY);
}

#define SE_GEOM_SET(type, srcArray, srcNum, copytag, dstArray, dstNum) \
do \
{ \
    delete[] dstArray; \
    if(copytag == SE_NOCOPY) \
    { \
        dstArray = srcArray; \
        dstNum = srcNum; \
    } \
    else \
    { \
        if(srcArray && srcNum) \
        { \
            SE_ARRAY_COPY(type, srcArray, srcNum, dstArray, dstNum); \
        } \
        else \
        {\
            dstArray = NULL;\
            dstNum = 0; \
        }\
    }\
} while(0)
void SE_GeometryData::setVertexes(SE_Vector3f* vertexArray, int vertexNum, COPY_TYPE copy)
{
    SE_GEOM_SET(SE_Vector3f, vertexArray, vertexNum, copy, this->vertexArray, this->vertexNum);
}
void SE_GeometryData::setFaces(SE_Face* faceArray, int faceNum , COPY_TYPE copy)
{
    SE_GEOM_SET(SE_Face, faceArray, faceNum, copy, this->faceArray, this->faceNum);
}
void SE_GeometryData::setTexVertexes(SE_Vector2f* texVertexArray, int texVertexNum, COPY_TYPE copy)
{ 
    SE_GEOM_SET(SE_Vector2f, texVertexArray, texVertexNum, copy , this->texVertexArray, this->texVertexNum);
}
void SE_GeometryData::setTexFaces(SE_Face* texFaceArray, int texFaceNum , COPY_TYPE copy)
{
    SE_GEOM_SET(SE_Face, texFaceArray, texFaceNum, copy, this->texFaceArray, this->texFaceNum);
}
void SE_GeometryData::setNormals(SE_Vector3f* normalArray, int normalNum, COPY_TYPE copy)
{
    SE_GEOM_SET(SE_Vector3f, normalArray, normalNum, copy, this->normalArray, this->normalNum);
}
void SE_GeometryData::setColors(SE_Vector3f* colorArray, int colorNum , COPY_TYPE copy)
{
    SE_GEOM_SET(SE_Vector3f, colorArray, colorNum, copy,this->colorArray, this->colorNum);
}
SE_GeometryData SE_GeometryData::createZAlignRect(float width, float height, float z)
{
    SE_GeometryData geomData;
    geomData.type = SE_TRIANGLES;
    geomData.vertexNum = 4;
    geomData.vertexArray = new SE_Vector3f[4];
    geomData.vertexArray[0] = SE_Vector3f(-width, -height, z);
    geomData.vertexArray[1] = SE_Vector3f(width, -height, z);
    geomData.vertexArray[2] = SE_Vector3f(width, height, z);
    geomData.vertexArray[3] = SE_Vector3f(-width, height, z);
    geomData.faceNum = 2;
    geomData.faceArray = new SE_Face[2];
    geomData.faceArray[0].v0 = 0;
    geomData.faceArray[0].v1 = 1;
    geomData.faceArray[0].v2 = 2;
    geomData.faceArray[1].v0 = 0;
    geomData.faceArray[1].v1 = 2;
    geomData.faceArray[1].v2 = 3;
    
    geomData.colorNum = 4;
    geomData.colorArray = new SE_Vector3f[4];
    float r = 1, g = 1, b = 1;
    geomData.colorArray[0] = SE_Vector3f(r, g, b);
    geomData.colorArray[1] = SE_Vector3f(r, g, b);
    geomData.colorArray[2] = SE_Vector3f(r, g, b);
    geomData.colorArray[3] = SE_Vector3f(r, g, b);
    return geomData;
}
SE_AABB SE_GeometryData::getAABB()
{
    if(aabb == NULL)
    {
        aabb = new SE_AABB;;
        aabb->createFromPoints(vertexArray, vertexNum);
    }
    return *aabb;
}
