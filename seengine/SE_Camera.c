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
    SE_Vec3f_Init(0, -1, 0, &upDir);
    if(SE_Vec3f_Compare(&upDir, &zDirNormal))
    {
        SE_Vec3f_Init(0, 0, 1, &upDir);
    }
    SE_Vector3f vPer, vParallel;
    SE_Vec3f_Decompose(&upDir, &zDirNormal, &vPer, &vParallel);
    SE_Vector3f xAxis, yAxis, zAxis;
    SE_Vec3f_Neg(&zDirNormal, &zAxis);
    SE_Vec3f_Normalize(&vPer, &yAxis);
    SE_Vec3f_Cross(&yAxis, &zAxis, &xAxis);
    SE_Vec3f_Copy(&xAxis, &out->xAxis);
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
{}
SE_Result SE_Camera_Update(SE_Camera* camera)
{}
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

