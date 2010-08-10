#ifndef SE_GEOMETRYDATA_H
#define SE_GEOMETRYDATA_H
class SE_Vector3f;
class SE_Vector3i;
class SE_Quat;
class SE_Matrix4f;
class SE_Matrix3f;
class SE_GeometryData
{
public:
    SE_GeometryData();
    ~SE_GeometryData();
    SE_Vector3f* getVertexArray()
    {
        return vertexArray;
    }
    int getVertexNum()
    {
        return vertexNum;
    }

    SE_Vector3i* getFaceArray()
    {
        return faceArray;
    }
    int getFaceNum()
    {
        return faceNum;
    }

    SE_Vector3f* getNormals();
    int getNormalsNum();

    void setVertexArray(SE_Vector3f* va, int num);
    void setFaceArray(SE_Vector3i* fa, int num);
    void setNormalArray(SE_Vector3f* na, int num);

    static void transform(SE_GeometryData* src, const SE_Matrix4f& m, SE_GeometryData* dst);
    static void transform(SE_GeometryData* src, const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate, SE_GeometryData* dst);
private:
    SE_GeometryData(const SE_GeometryData&);
    SE_GeometryData& operator=(const SE_GeometryData&);
private:
    SE_Vector3f* vertexArray;
    int vertexNum;
    SE_Vector3i* faceArray;
    int faceNum;
    SE_Vector3f* normalArray;
    int normalNum;
};
#endif
