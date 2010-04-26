#include "SE_GeometryData.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#undef SE_ARRAY_COPY
#undef SE_ARRAY_FREE
#define SE_ARRAY_COPY(type, srcArray, srcArrayNum, elementSize, dstArray, dstArrayNum) do{ \
    (dstArray) = (type)SE_Malloc((srcArrayNum) * (elementSize)); \
    if((dstArray)) \
    { \
        (dstArrayNum) = (srcArrayNum); \
        int i; \
        for(i = 0 ; i < (srcArrayNum) ; i++) \
        { \
            (dstArray)[i] = (srcArray)[i]; \
        } \
    } \
}while(0)
#define SE_ARRAY_FREE(own, array) do{ \
    if((own) && (array)) \
    { \
        SE_Free((array)); \
    } \
}while(0)
SE_Result SE_GeometryData_Init(int type, SE_Vector3f* vertexArray, int vertexNum, int ownVertexArray,
                                      SE_Vector3f* texVertexArray, int texVertexNum, int ownTexVertexArray,
                                      SE_Face* faceArray, int faceNum, int ownFaceArray,
                                      SE_Face* texFaceArray, int texFaceNum, int ownTexFaceArray,
                                      SE_Vector3f* normalArray, int normalNum, int ownNormalArray, 
                                      SE_Vector3f* colorArray, int colorNum, int ownColorArray,SE_GeometryData* out)
{
    SE_ASSERT(out);
    SE_Object_Clear(out, sizeof(SE_GeometryData));
    out->type = type;
    if(!ownVertexArray)
    {
        out->vertexArray = vertexArray;
        out->vertexNum = vertexNum;
    }
    else
    {
        if(vertexNum > 0 && vertexArray)
        {
            SE_ARRAY_COPY(SE_Vector3f*, vertexArray, vertexNum, sizeof(SE_Vector3f),out->vertexArray, out->vertexNum);
        }
    }
    out->ownVertexArray = ownVertexArray;
    if(!ownTexVertexArray)
    {
        out->texVertexArray = texVertexArray;
        out->texVertexNum = texVertexNum;
    }
    else
    {
        if(texVertexArray && texVertexNum > 0)
        {
            SE_ARRAY_COPY(SE_Vector3f*, texVertexArray, texVertexNum, sizeof(SE_Vector3f), out->texVertexArray, out->texVertexNum);
        }
    }
    out->ownTexVertexArray = ownTexVertexArray;
    if(!ownNormalArray)
    {
        out->normalArray = normalArray;
        out->normalNum = normalNum;
    }
    else
    {
        if(normalArray && normalNum > 0)
        {
            SE_ARRAY_COPY(SE_Vector3f*, normalArray, normalNum, sizeof(SE_Vector3f), out->normalArray, out->normalNum);
        }
    }
    out->ownNormalArray = ownNormalArray;
    if(!ownFaceArray)
    {
        out->faceArray = faceArray;
        out->faceNum = faceNum;
    }
    else
    {
        if(faceArray && faceNum > 0)
        {
            SE_ARRAY_COPY(SE_Face*, faceArray, faceNum, sizeof(SE_Face), out->faceArray, out->faceNum);
        }
    }
    out->ownFaceArray = ownFaceArray;
    if(!ownTexFaceArray)
    {
        out->texFaceArray = texFaceArray;
        out->texFaceNum = texFaceNum;
    }
    else
    {
        if(texFaceArray && texFaceNum > 0)
        {
            SE_ARRAY_COPY(SE_Face*, texFaceArray, texFaceNum, sizeof(SE_Face), out->texFaceArray, out->texFaceNum);
        }
    }
    out->ownTexFaceArray = ownTexFaceArray;
    if(!ownColorArray)
    {
        out->colorArray = colorArray;
        out->colorNum = colorNum;
    }
    else
    {
        if(colorArray && colorNum > 0)
        {
            SE_ARRAY_COPY(SE_Vector3f*, colorArray, colorNum, sizeof(SE_Vector3f),out->colorArray, out->colorNum);
        }
    }
    out->ownColorArray = ownColorArray;
    return SE_VALID;

}
void SE_GeometryData_Release(void* gd)
{
    SE_ASSERT(gd);
    SE_GeometryData* data = (SE_GeometryData*)gd;
    if(data->ownVertexArray && data->vertexArray != NULL)
    {
        SE_Free(data->vertexArray);
    }
    if(data->ownTexVertexArray && data->texVertexArray != NULL)
    {
        SE_Free(data->texVertexArray);
    }
    if(data->ownNormalArray && data->normalArray)
    {
        SE_Free(data->normalArray);
    }
    if(data->ownFaceArray && data->faceArray)
    {
        SE_Free(data->faceArray);
    }
    if(data->ownTexFaceArray && data->texFaceArray)
    {
        SE_Free(data->texFaceArray);
    }
    if(data->ownColorArray && data->colorArray)
    {
        SE_Free(data->colorArray);
    }
}
SE_Result SE_GeometryData_Copy(const SE_GeometryData* gdSrc, SE_GeometryData* gdDst)
{
    SE_ASSERT(gdSrc);
    SE_ASSERT(gdDst);
    SE_Object_Clear(gdDst, sizeof(SE_GeometryData));
    return SE_GeometryData_Init(gdSrc->type, gdSrc->vertexArray, gdSrc->vertexNum, 1, gdSrc->texVertexArray, gdSrc->texVertexNum, 1, gdSrc->faceArray, gdSrc->faceNum, 1, gdSrc->texFaceArray, gdSrc->texFaceNum, 1, gdSrc->normalArray, gdSrc->normalNum, 1, gdSrc->colorArray, gdSrc->colorNum, 1, gdDst);
   
}
SE_Result SE_FaceList_Init(SE_FaceList* fl, SE_GeometryData* s, int faceNum, int* faces)
{
    SE_ASSERT(fl);
    fl->source = s;
    fl->faces = faces;
    fl->num = faceNum;
    return SE_VALID;
}
SE_Result SE_FaceList_Release(void* fl)
{
    SE_FaceList* facelist = (SE_FaceList*)fl;
    if(facelist->num > 0)
    {
        SE_Free(facelist->faces);
    }
}
SE_Result SE_GeometryData_SetVertexes(SE_GeometryData* gd, SE_Vector3f* vertexArray, int vertexNum, int isCopy)
{
    SE_ASSERT(gd);
    if(vertexArray == NULL || vertexNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->vertexArray = vertexArray;
        gd->vertexNum = vertexNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Vector3f*, vertexArray, vertexNum, sizeof(SE_Vector3f),gd->vertexArray, gd->vertexNum);
    }
    gd->ownVertexArray = 1;
    return SE_VALID;
}
SE_Result SE_GeometryData_SetFaces(SE_GeometryData* gd, SE_Face* faceArray, int faceNum , int isCopy)
{
    SE_ASSERT(gd);
    if(faceArray == NULL || faceNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->faceArray = faceArray;
        gd->faceNum = faceNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Face*, faceArray, faceNum, sizeof(SE_Face), gd->faceArray, gd->faceNum);

    }
    gd->ownFaceArray = 1;
    return SE_VALID;
}
SE_Result SE_GeometryData_SetTexVertexes(SE_GeometryData* gd, SE_Vector3f* texVertexArray, int texVertexNum, int isCopy)
{
    SE_ASSERT(gd);
    if(texVertexArray == NULL || texVertexNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->texVertexArray = texVertexArray;
        gd->texVertexNum = texVertexNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Vector3f*, texVertexArray, texVertexNum, sizeof(SE_Vector3f), gd->texVertexArray, gd->texVertexNum);

    }
    gd->ownTexVertexArray = 1;
    return SE_VALID;
}
SE_Result SE_GeometryData_SetTexFaces(SE_GeometryData* gd, SE_Face* texFaceArray, int texFaceNum , int isCopy)
{
    SE_ASSERT(gd);
    if(texFaceArray == NULL || texFaceNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->texFaceArray = texFaceArray;
        gd->texFaceNum = texFaceNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Face*, texFaceArray, texFaceNum, sizeof(SE_Face), gd->texFaceArray, gd->texFaceNum);

    }
    gd->ownTexFaceArray = 1;
    return SE_VALID;
}
SE_Result SE_GeometryData_SetNormals(SE_GeometryData* gd, SE_Vector3f* normalArray, int normalNum, int isCopy)
{
    SE_ASSERT(gd);
    if(normalArray == NULL || normalNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->normalArray = normalArray;
        gd->normalNum = normalNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Vector3f*, normalArray, normalNum, sizeof(SE_Vector3f), gd->normalArray, gd->normalNum);

    }
    gd->ownNormalArray = 1;
    return SE_VALID;
}
SE_Result SE_GeometryData_SetColors(SE_GeometryData* gd, SE_Vector3f* colorArray, int colorNum , int isCopy)
{
    SE_ASSERT(gd);
    if(colorArray == NULL || colorNum <= 0)
        return SE_INVALID;
    if(!isCopy)
    {
        gd->colorArray = colorArray;
        gd->colorNum = colorNum;
    }
    else
    {
        SE_ARRAY_COPY(SE_Vector3f*, colorArray, colorNum, sizeof(SE_Vector3f), gd->colorArray, gd->colorNum);

    }
    gd->ownColorArray = 1;
    return SE_VALID;
}

