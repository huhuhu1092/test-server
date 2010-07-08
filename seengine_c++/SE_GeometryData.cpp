#include "SE_GeometryData.h"
#include <SE_Vector.h>
SE_GeometryData::SE_GeometryData()
{
    vertexArray = NULL;
    vertexNum = 0;
    faceArray = NULL;
    faceNum = 0;
    normalArray = NULL;
    normalNum = 0;
}
SE_GeometryData::~SE_GeometryData()
{
    if(vertexArray)
        delete[] vertexArray;
    if(faceArray)
        delete[] faceArray;
    if(normalArray)
        delete[] normalArray;
}

SE_Vector3f* SE_GeometryData::getNormals()
{

}
int SE_GeometryData::getNormalsNum()
{}

void SE_GeometryData::setVertexArray(SE_Vector3f* va, int num)
{
    if(vertexArray)
        delete[] vertexArray;
    vertexArray = va;
    vertexNum = num;
}
void SE_GeometryData::setFaceArray(SE_Vector3i* fa, int num)
{
    if(faceArray)
        delete[] faceArray;
    faceArray = fa;
    faceNum = num;
}
void SE_GeometryData::setNormalArray(SE_Vector3f* na, int num)
{
    if(normalArray)
        delete[] normalArray;
    normalArray = na;
    normalNum = num;
}

