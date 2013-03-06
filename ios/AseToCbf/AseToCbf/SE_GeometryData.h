#ifndef SE_GEOMETRYDATA_H
#define SE_GEOMETRYDATA_H
#include "SE_Common.h"
#include "SE_Vector.h"

#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_Face_tag
{
    union
    {
        int v[3];
        struct
        {
            int v0, v1, v2;
        };
    };
} SE_Face;
enum SE_GEOM_TYPE {SE_TRIANGLES, SE_TRIANGLE_FANS, SE_TRIANGLE_TRIP, SE_LINES, SE_POINTS};
typedef struct SE_Vertex_XYZUV_tag
{
    float x, y, z;
    float u, v;
} SE_Vertex_XYZUV;
typedef struct SE_VertexBuffer_tag
{
    int vertexNum;
    SE_Vertex_XYZUV* vertexArray;
    int faceNum;
    int* faceArray;
    
} SE_VertexBuffer;
typedef struct SE_MapChannel_tag
{
    int numTVertex;
    SE_Vector2f* texVertexArray;
    int numTFaces;
    SE_Face* texFaceArray;
} SE_MapChannel;
typedef struct SE_GeometryData_tag
{
    int type;
    SE_Vector3f* vertexArray;
    int vertexNum;
    int ownVertexArray;
    SE_Vector3f* texVertexArray;
    int texVertexNum;
    int ownTexVertexArray;
    SE_Face* texFaceArray; 
    int texFaceNum;
    int ownTexFaceArray;
    SE_Vector3f* normalArray;
    int normalNum;
    int ownNormalArray;
    SE_Face* faceArray;
    int faceNum;
    int ownFaceArray;
    SE_Vector3f* colorArray;
    int colorNum;
    int ownColorArray;
    
    int numMapChannel;
    SE_MapChannel* mapChannelArray;
    int ownMapChannel;
    
    int texVertexNum2;
    SE_Vector3f* texVertexArray2;
    int ownTexVertexArray2;
    
    int texFaceNum2;
    SE_Face* texFaceArray2;
    int ownTexFaceArray2;
} SE_GeometryData;
typedef struct SE_FaceList_tag
{
    SE_GeometryData* source;// this field need not free when release
    int* faces; // the index array of face in source data
    int num;
} SE_FaceList;
extern SE_Result SE_FaceList_Init(SE_FaceList* fl, SE_GeometryData* s, int faceNum, int* faces);
extern void SE_FaceList_Release(void* fl);

/**
 * if own** is true, SE_GeometryData_Init will copy  the *Array to out
 * **/
extern SE_Result SE_GeometryData_Init(int type, SE_Vector3f* vertexArray, int vertexNum, int ownVertexArray,
                                      SE_Vector3f* texVertexArray, int texVertexNum, int ownTexVertexArray,
                                      SE_Face* faceArray, int faceNum, int ownFaceArray,
                                      SE_Face* texFaceArray, int texFaceNum, int ownTexFaceArray,
                                      SE_Vector3f* normalArray, int normalNum, int ownNormalArray, 
                                      SE_Vector3f* colorArray, int colorNum, int ownColorArray, SE_GeometryData* out);
extern SE_Result SE_GeometryData_SetVertexes(SE_GeometryData* gd, SE_Vector3f* vertexArray, int vertexNum, int isCopy);
extern SE_Result SE_GeometryData_SetFaces(SE_GeometryData* gd, SE_Face* faceArray, int faceNum , int isCopy);
extern SE_Result SE_GeometryData_SetTexVertexes(SE_GeometryData* gd, SE_Vector3f* texVertexArray, int texVertexNum, int isCopy);
extern SE_Result SE_GeometryData_SetTexFaces(SE_GeometryData* gd, SE_Face* texFaceArray, int texFaceNum , int isCopy);
extern SE_Result SE_GeometryData_SetNormals(SE_GeometryData* gd, SE_Vector3f* normalArray, int normalNum, int isCopy);
extern SE_Result SE_GeometryData_SetColors(SE_GeometryData* gd, SE_Vector3f* colorArray, int colorNum , int isCopy);
extern void SE_GeometryData_Release(void* gd);
extern SE_Result SE_GeometryData_Copy(const SE_GeometryData* gdSrc, SE_GeometryData* gdDst);

#ifdef __cplusplus
}
#endif
#endif
