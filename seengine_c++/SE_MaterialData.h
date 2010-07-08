#ifndef SE_MATERIALDATA_H
#define SE_MATERIALDATA_H
#include "SE_Vector.h"
class SE_MaterialData
{
public:
    SE_Vector3f ambient;
    SE_Vector3f diffuse;
    SE_Vector3f specular;
    SE_Vector3f shiny;
};
#endif
