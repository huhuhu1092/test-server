#include "SE_Camera.h"
#include "SE_Log.h"
SE_Result SE_Camera_InitByLocationTarget(const SE_Vector3f* location, const SE_Vector3f* target, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
    SE_Vector3f zDir;
    SE_Vec3f_Subtract(target, location, &zDir);
    if(SE_Vec3f_IsZero(&zDir))
    {
        LOGI("camera z direction is zero\n");
        SE_Vec3f_Init(0, 0, -1, &zDir);
    }
    SE_Vector3f zDirNormal;
    SE_Vec3f_Normalize(&zDir, &zDirNormal);
    SE_Vector3f upDir;
    SE_Vec3f_Init(0, 1, 0, &upDir);
    if(SE_Vec3f_Compare(&upDir, &zDirNormal))
    {
        SE_Vec3f_Init(0, 0, 1, &upDir);
    }
    SE_Vector3f xAxis, yAxis, zAxis;
    SE_Vec3f_Neg(&zDirNormal, &zAxis);
    SE_Vector3f x, y;
    SE_Vec3f_Cross(&upDir, &zDirNormal, &x);
    SE_Vec3f_Cross(&zDirNormal, &x, &y);
    SE_Vec3f_Normalize(&y, &yAxis);
    SE_Vec3f_Normalize(&x, &xAxis);

    SE_Vec3f_Neg(&xAxis, &out->xAxis);
    SE_Vec3f_Copy(&yAxis, &out->yAxis);
    SE_Vec3f_Copy(&zAxis, &out->zAxis);
    SE_Vec3f_Copy(location, &out->location);
    return SE_VALID;
}
SE_Result SE_Camera_InitByFrame(const SE_Vector3f* location, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Vec3f_Normalize(xAxis, &out->xAxis);
    SE_Vec3f_Normalize(yAxis, &out->yAxis);
    SE_Vec3f_Normalize(zAxis, &out->zAxis);
    SE_Vec3f_Copy(location, &out->location);
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
    return SE_VALID;
}
SE_Result SE_Camera_InitByDirectionUp(const SE_Vector3f* location, const SE_Vector3f* zAxis, const SE_Vector3f* yAxis, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Vec3f_Normalize(zAxis, &out->zAxis);
    SE_Vec3f_Normalize(yAxis, &out->yAxis);
    SE_Vec3f_Cross(&out->yAxis, &out->zAxis, &out->xAxis);
    SE_Vec3f_Copy(location, &out->location);
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
    return SE_VALID;
}

SE_Result SE_Camera_CullBouningVolume(const SE_Camera* camera, SE_BoundingVolume* bv)
{
    return SE_VALID;
}
SE_Result SE_Camera_Update(SE_Camera* camera)
{
    return SE_VALID;
}
SE_Result SE_Camera_SetViewport(SE_Camera* camera, int x, int y, int w, int h)
{
    camera->viewport.left = x;
    camera->viewport.top = y;
    camera->viewport.right = x + w;
    camera->viewport.bottom = y + h;
    return SE_VALID;
    
}
SE_Result SE_Camera_GetMatrixWorldToView(SE_Camera* camera, SE_Matrix4f* out)
{
    SE_Matrix4f vtow;
    SE_Camera_GetMatrixViewToWorld(camera, &vtow);
    SE_Mat4f_Inverse(&vtow, out);
    return SE_VALID; 
}
SE_Result SE_Camera_GetMatrixViewToWorld(SE_Camera* camera, SE_Matrix4f* out)
{
    SE_Matrix3f rsm;
    SE_Mat3f_InitFromColumn(&camera->xAxis, &camera->yAxis, &camera->zAxis, &rsm);
    SE_Mat4f_InitFromMT(&rsm, &camera->location, out);
    return SE_VALID;
}
SE_Result SE_Camera_RotateLocalXYZAxis(SE_Camera* camera, float rotateAngle, int axis)
{
    SE_Matrix3f initm , resultm;
    SE_Matrix3f rotatem;/*this is the rotate matrix*/
    SE_Vector3f initAxis, resultAxis;
    SE_Matrix4f viewToWorld;
    SE_Vector4f axisByV4, resultAxisByV4;/*indicate axis with vector4f*/
    SE_Vector3f xAxis, yAxis, zAxis;
    /*implement*/
    if(axis < 0 || axis > 2)
    {
        LOGI("rotate camera axis invalid\n");
        return SE_INVALID;
    }
    SE_Mat3f_InitFromColumn(&camera->xAxis, &camera->yAxis, &camera->zAxis, &initm);
    switch(axis)
    {
    case 0: /*when rotate about x axis we use z axis to do transform*/
        SE_Vec3f_Init(0, 0, 1, &initAxis);
        SE_Mat3f_RotateX(rotateAngle, &rotatem); 
        break;
    case 1:/*when rotate about y axis we use z axis to do transform */
        SE_Vec3f_Init(0, 0, 1, &initAxis);
        SE_Mat3f_RotateY(rotateAngle, &rotatem);
        break;
    case 2:/*when rotate about z axis we use y axis to do transform */
        SE_Vec3f_Init(0, 1, 0, &initAxis);
        SE_Mat3f_RotateZ(rotateAngle, &rotatem);
        break;
    }
    SE_Mat3f_Map(&rotatem, &initAxis, &resultAxis);
    SE_Camera_GetMatrixViewToWorld(camera, &viewToWorld);
    SE_Vec4f_Init(resultAxis.x, resultAxis.y, resultAxis.z, 0.0f, &axisByV4);
    SE_Mat4f_Map(&viewToWorld, &axisByV4, &resultAxisByV4);
    switch(axis)
    {
    case 0:
        SE_Vec3f_Init(resultAxisByV4.x, resultAxisByV4.y, resultAxisByV4.z, &zAxis);
        SE_Vec3f_Copy(&camera->xAxis, &xAxis);
        SE_Vec3f_Cross(&zAxis, &xAxis, &yAxis);
        break;
    case 1:
        SE_Vec3f_Init(resultAxisByV4.x, resultAxisByV4.y, resultAxisByV4.z, &zAxis);
        SE_Vec3f_Copy(&camera->yAxis, &yAxis);
        SE_Vec3f_Cross(&yAxis, &zAxis, &xAxis);
        break;
    case 2:
        SE_Vec3f_Init(resultAxisByV4.x, resultAxisByV4.y, resultAxisByV4.z, &yAxis);
        SE_Vec3f_Copy(&camera->zAxis, &zAxis);
        SE_Vec3f_Cross(&yAxis, &zAxis, &xAxis);
        break;
    }
    SE_Vec3f_Normalize(&xAxis, &camera->xAxis);
    SE_Vec3f_Normalize(&yAxis, &camera->yAxis);
    SE_Vec3f_Normalize(&zAxis, &camera->zAxis); 
    return SE_VALID;
}
SE_Result SE_Camera_LocationTranslateAlignXYZ(SE_Camera* camera, float translate, int axis)
{
    SE_Vector3f newLoc;
    SE_Vector3f delta;
    if(translate == 0.0)
        return SE_VALID;
    switch(axis)
    {
    case 0:
        SE_Vec3f_Mul(&camera->xAxis, translate, &delta);
        break;
    case 1:
        SE_Vec3f_Mul(&camera->yAxis, translate, &delta);
        break;
    case 2:
        SE_Vec3f_Mul(&camera->zAxis, translate, &delta);
        break;
    }
    
    SE_Vec3f_Add(&camera->location, &delta, &newLoc);
    SE_Vec3f_Copy(&newLoc, &camera->location);
    return SE_VALID;
}
