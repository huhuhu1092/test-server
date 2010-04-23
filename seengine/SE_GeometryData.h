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
} SE_GeometryData;
typedef struct SE_FaceList_tag
{
    SE_GeometryData* source;// this field need not free when release
    int* faces; // the index array of face in source data
    int num;
} SE_FaceList;
extern SE_Result SE_FaceList_Init(SE_FaceList* fl, SE_GeometryData* s, int faceNum, int* faces);
extern SE_Result SE_FaceList_Release(void* fl);

/****/
extern SE_Result SE_GeometryData_Init(int type, SE_Vector3f* vertexArray, int vertexNum, int ownVertexArray,
                                      SE_Vector3f* texVertexArray, int texVertexNum, int ownTexVertexArray,
                                      SE_Face* faceArray, int faceNum, int ownFaceArray,
                                      SE_Face* texFaceArray, int texFaceNum, int ownTexFaceArray,
                                      SE_Vector3f* normalArray, int normalNum, int ownNormalArray, 
                                      SE_Vector3f* colorArray, int colorNum, int ownColorArray, SE_GeometryData* out);
extern void SE_GeometryData_Release(void* gd);
extern SE_Result SE_GeometryData_Copy(const SE_GeometryData* gdSrc, SE_GeometryData* gdDst);

#ifdef __cplusplus
}
#endif
#endif
