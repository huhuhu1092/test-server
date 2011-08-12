#include "SE_GeometryData.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_MemLeakDetector.h"
SE_GeometryData::SE_GeometryData()
{
    vertexArray = NULL;
    vertexNum = 0;
    mOwnVertex = true;
    faceArray = NULL;
    faceNum = 0;
    mOwnFace = true;
    normalArray = NULL;
    normalNum = 0;
    mOwnNormal = true;
}
SE_GeometryData::~SE_GeometryData()
{
    if(vertexArray && mOwnVertex)
        delete[] vertexArray;
    if(faceArray && mOwnFace)
        delete[] faceArray;
    if(normalArray && mOwnNormal)
        delete[] normalArray;
}

SE_Vector3f* SE_GeometryData::getNormals()
{
    return 0;
}
int SE_GeometryData::getNormalsNum()
{
    return 0;
}

void SE_GeometryData::setVertexArray(SE_Vector3f* va, int num, bool own)
{
    if(vertexArray && mOwnVertex)
        delete[] vertexArray;
    vertexArray = va;
    vertexNum = num;
    mOwnVertex = own;
}
void SE_GeometryData::setFaceArray(SE_Vector3i* fa, int num, bool own)
{
    if(faceArray && mOwnFace)
        delete[] faceArray;
    faceArray = fa;
    faceNum = num;
    mOwnFace = own;
}
/**************  add for particle    ***************/
void SE_GeometryData::setParticleVertexArray(SE_Vector3f* va, int num, bool own)
{
    vertexArray = va;
    vertexNum = num;
    mOwnVertex = own;
}
/**************  add for particle    ***************/
void SE_GeometryData::setParticleFaceArray(SE_Vector3i* fa, int num, bool own)
{
    faceArray = fa;
    faceNum = num;
    mOwnFace = own;
}
void SE_GeometryData::setNormalArray(SE_Vector3f* na, int num, bool own)
{
    if(normalArray && mOwnNormal)
        delete[] normalArray;
    normalArray = na;
    normalNum = num;
    mOwnNormal = own;
}
void SE_GeometryData::transform(SE_GeometryData* src, const SE_Matrix4f& m, SE_GeometryData* dst)
{
    SE_Vector3f* vertex = NULL;
    SE_Vector3i* faces = NULL;
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
    /*
    if(src->faceArray)
    {
        faces = new SE_Vector3i[src->faceNum];
        for(int i = 0 ; i < src->faceNum; i++)
        {
            faces[i] = src->faceArray[i];
        }
        faceNum = src->faceNum;
    }
    */
    if(src->normalArray)
    {
        SE_Matrix3f t = m.toMatrix3f();
        if(t.hasInverse())
        {
            SE_Matrix3f inverse = t.inverse();
            inverse = inverse.transpose();
            normal = new SE_Vector3f[src->normalNum];
            for(int i = 0 ; i < src->normalNum ; i++)
            {
                normal[i] = inverse.map(src->normalArray[i]);
            }
            normalNum = src->normalNum;
        }
    }
    dst->setVertexArray(vertex, vertexNum);
    dst->setFaceArray(src->faceArray, src->faceNum, false);
    dst->setNormalArray(normal, normalNum);
}
void SE_GeometryData::transform(SE_GeometryData* src, const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate, SE_GeometryData* dst)
{
    SE_Vector3f* vertex = NULL;
    SE_Vector3i* faces = NULL;
    SE_Vector3f* normal = NULL;
    int vertexNum = 0;
    int faceNum = 0;
    int normalNum = 0;
    if(src->vertexArray)
    {
        vertex = new SE_Vector3f[src->vertexNum];
        for(int i = 0 ; i < src->vertexNum ; i++)
        {
            SE_Vector3f v = scale.mul(src->vertexArray[i]);
            v = rotate.map(v);
            v = v + translate;
            vertex[i] = v;
        }
        vertexNum = src->vertexNum;
    }
    /*
    if(src->faceArray)
    {
        faces = new SE_Vector3i[src->faceNum];
        for(int i = 0 ; i < src->faceNum; i++)
        {
            faces[i] = src->faceArray[i];
        }
        faceNum = src->faceNum;
    }
    */
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
            normal = new SE_Vector3f[src->normalNum];
            for(int i = 0 ; i < src->normalNum ; i++)
            {
                normal[i] = m.map(src->normalArray[i]);
            }
            normalNum = src->normalNum;
        }
    }
    dst->setVertexArray(vertex, vertexNum);
    dst->setFaceArray(src->faceArray, src->faceNum, false);
    dst->setNormalArray(normal, normalNum);
}

