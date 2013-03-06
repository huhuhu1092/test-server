#ifndef SE_MESH_H
#define SE_MESH_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_GeometryData.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_Material_tag
{
    SE_Vector3f ambient;
    SE_Vector3f diffuse;
    SE_Vector3f specular;
    SE_String texturename;
} SE_Material;
typedef struct SE_Mesh_tag
{
    SE_GeometryData* geomData;
    SE_Material* material;
    SE_Vector3f defaultColor;
} SE_Mesh;
#ifdef __cplusplus
}
#endif
#endif
