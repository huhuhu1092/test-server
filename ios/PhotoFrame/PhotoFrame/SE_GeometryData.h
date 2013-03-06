#ifndef SE_GEOMETRYDATA_H
#define SE_GEOMETRYDATA_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Geometry3D.h"
struct SE_Face
{
    union
    {
        int v[3];
        struct
        {
            int v0, v1, v2;
        };
    };
    SE_Face()
    {
        v0 = v1 = v2 = 0;
    }
    
} ;
struct SE_MapChannel
{
    int numTVertex;
    SE_Vector2f* texVertexArray;
    int numTFaces;
    SE_Face* texFaceArray;
};
struct SE_Vertex_XYZUV
{
    float x, y, z;
    float u, v;
};
struct SE_GeometryData
{
    enum SE_GEOM_TYPE {SE_TRIANGLES, SE_TRIANGLE_FANS, SE_TRIANGLE_TRIP, SE_LINES, SE_POINTS};
    enum COPY_TYPE {SE_COPY, SE_NOCOPY};
    int type;
    SE_Vector3f* vertexArray;
    int vertexNum;
    SE_Vector2f* texVertexArray;
    int texVertexNum;
    SE_Face* texFaceArray; 
    int texFaceNum;
    SE_Vector3f* normalArray;
    int normalNum;
    SE_Face* faceArray;
    int faceNum;
    SE_Vector3f* colorArray;
    int colorNum;
    
    SE_Vertex_XYZUV* vertexBufferArray;
    int vertexBufferNum;
    unsigned short* vertexBufferFaceArray;
    
    SE_MapChannel* mapChannelArray;
    int mapChannelNum;
    
    SE_Vector2f* texVertexArray2;
    int texVertexNum2;
    
    SE_Face* texFaceArray2;
    int texFaceNum2;
    SE_AABB* aabb;
public:
    void resetData()
    {
        type = SE_TRIANGLES;
        vertexArray = NULL;
        vertexNum = 0;
        texVertexNum = 0;
        texVertexArray = NULL;
        texFaceNum = 0;
        texFaceArray = NULL;
        normalArray = NULL;
        normalNum = 0;
        faceArray = NULL;
        faceNum = 0;
        colorArray = NULL;
        colorNum = 0;
        mapChannelArray = NULL;
        mapChannelNum = 0;
        
        texVertexArray2 = NULL;
        texVertexNum2 = 0;
        texFaceArray2 = NULL;
        texFaceNum2 = 0;
        aabb = NULL;
        
        vertexBufferNum = 0;
        vertexBufferArray = NULL;
        vertexBufferFaceArray = NULL;
    }
    SE_GeometryData(bool own = true)
    {
        resetData();
    }
    ~SE_GeometryData();
    SE_GeometryData(SE_GEOM_TYPE type, SE_Vector3f* vertexArray, int vertexNum,
                    SE_Vector2f* texVertexArray, int texVertexNum,
                    SE_Face* faceArray, int faceNum,
                    SE_Face* texFaceArray, int texFaceNum,
                    SE_Vector3f* normalArray, int normalNum,
                    SE_Vector3f* colorArray, int colorNum);
    void setVertexes(SE_Vector3f* vertexArray, int vertexNum, COPY_TYPE copy);
    void setFaces(SE_Face* faceArray, int faceNum, COPY_TYPE copy);
    void setTexVertexes(SE_Vector2f* texVertexArray, int texVertexNum, COPY_TYPE copy);
    void setTexFaces(SE_Face* texFaceArray, int texFaceNum, COPY_TYPE copy);
    void setNormals(SE_Vector3f* normalArray, int normalNum, COPY_TYPE copy);
    void setColors(SE_Vector3f* colorArray, int colorNum , COPY_TYPE copy);
    void clone(SE_GeometryData* outData);
    void release();
    SE_AABB getAABB();
    static SE_GeometryData createZAlignRect(float width, float height, float z);
};

#endif
