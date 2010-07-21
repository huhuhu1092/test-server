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
void SE_GeometryData::transform(SE_GeometryData* src, const SE_Matrix4f& m, SE_GeometryData* dst)
{
    SE_Vector3f* vertex = NULL;
    SE_Vector3f* face = NULL;
    SE_Vector3f* normal = NULL;
    int vertexNum = 0;
    int faceNum = 0;
    int normalNum = 0;
    if(src->vertexArray)
    {
        vertex = new SE_Vector3f[src->vertexNum];
        for(int i = 0 ; i < src->vertexNum ; i++)
        {
            SE_Vector4f v(src->vertexArray[i], 1.0f);
            v = m.map(v);
            vertex[i] = v.xyz();
        }
        vertexNum = src->vertexNum;
    }
    if(src->faceArray)
    {
        SE_Vector3i* faces = new SE_Vector3i[src->faceNum];
        for(int i = 0 ; i < src->faceNum)
        {
            faces[i] = src->faceArray[i];
        }
        faceNum = src->faceNum;
    }
    if(src->normalArray)
    {
        SE_Matrix3f t = m.toMatrix3f();
        if(t.hasInverse())
        {
            SE_Matrix3f inverse = t.inverse();
            inverse = inverse.transpose();
            normal = new SE_Vector3f[src->normalNum]
            for(int i = 0 ; i < src->normalNum ; i++)
            {
                normal[i] = inverse.map(src->normalArray[i]);
            }
            normalNum = src->normalNum;
        }
    }
    dst->setVertexArray(vertex, vertexNum);
    dst->setFaceArray(face, faceNum);
    dst->setNormalArray(normal, normalNum);
}
void SE_GeometryData::transform(SE_GeometryData* src, const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
    SE_Vector3f* vertex = NULL;
    SE_Vector3f* face = NULL;
    SE_Vector3f* normal = NULL;
    int vertexNum = 0;
    int faceNum = 0;
    int normalNum = 0;
    if(src->vertexArray)
    {
        vertex = new SE_Vector3f[src->vertexNum];
        for(int i = 0 ; i < src->vertexNum ; i++)
        {
            SE_Vector3f v = scale.dot(src->vertexArray[i]);
            v = rotate.map(v);
            v = v + translate;
            vertex[i] = v;
        }
        vertexNum = src->vertexNum;
    }
    if(src->faceArray)
    {
        SE_Vector3i* faces = new SE_Vector3i[src->faceNum];
        for(int i = 0 ; i < src->faceNum)
        {
            faces[i] = src->faceArray[i];
        }
        faceNum = src->faceNum;
    }
    if(src->normalArray)
    {
        SE_Matrix3f t;
        t.setScale(scale.x, scale.y, scale.z);
        SE_Matrix3f m;
        m = rotate.toMatrix3f();
        m = m.mul(t);
        if(m.hasInverse())
        {
            SE_Matrix3f inverse = m.inverse();
            m = m.transpose();
            normal = new SE_Vector3f[src->normalNum]
            for(int i = 0 ; i < src->normalNum ; i++)
            {
                normal[i] = m.map(src->normalArray[i]);
            }
            normalNum = src->normalNum;
        }
    }
    dst->setVertexArray(vertex, vertexNum);
    dst->setFaceArray(face, faceNum);
    dst->setNormalArray(normal, normalNum);
}

