#include "SE_Camera.h"
#include "SE_Log.h"
#include "SE_Math.h"
SE_Result SE_Camera_InitByLocationTarget(const SE_Vector3f* location, const SE_Vector3f* target, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Vector3f zDir;
    SE_Vector3f zDirNormal;
    SE_Vector3f upDir;
    SE_Vector3f xAxis, yAxis, zAxis;
    SE_Vector3f x, y;
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
    SE_Vec3f_Subtract(target, location, &zDir);
    if(SE_Vec3f_IsZero(&zDir))
    {
        LOGI("camera z direction is zero\n");
        SE_Vec3f_Init(0, 0, -1, &zDir);
    }
    SE_Vec3f_Normalize(&zDir, &zDirNormal);
    SE_Vec3f_Init(0, 1, 0, &upDir);
    if(SE_Vec3f_Compare(&upDir, &zDirNormal))
    {
        SE_Vec3f_Init(0, 0, 1, &upDir);
    }
    SE_Vec3f_Neg(&zDirNormal, &zAxis);
    SE_Vec3f_Cross(&upDir, &zDirNormal, &x);
    SE_Vec3f_Cross(&zDirNormal, &x, &y);
    SE_Vec3f_Normalize(&y, &yAxis);
    SE_Vec3f_Normalize(&x, &xAxis);

    SE_Vec3f_Neg(&xAxis, &out->xAxis);
    SE_Vec3f_Copy(&yAxis, &out->yAxis);
    SE_Vec3f_Copy(&zAxis, &out->zAxis);
    SE_Vec3f_Copy(location, &out->location);
	out->changed = 1;
    return SE_VALID;
}
SE_Result SE_Camera_InitByFrame(const SE_Vector3f* location, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Vec3f_Normalize(xAxis, &out->xAxis);
    SE_Vec3f_Normalize(yAxis, &out->yAxis);
    SE_Vec3f_Normalize(zAxis, &out->zAxis);
    SE_Vec3f_Copy(location, &out->location);
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
    out->changed = 1;
    return SE_VALID;
}
SE_Result SE_Camera_InitByDirectionUp(const SE_Vector3f* location, const SE_Vector3f* zAxis, const SE_Vector3f* yAxis, float fov, float ratio, float near, float far, SE_Camera* out)
{
    SE_Vector3f xAxis;
    SE_Vec3f_Normalize(zAxis, &out->zAxis);
    SE_Vec3f_Normalize(yAxis, &out->yAxis);
    SE_Vec3f_Cross(&out->yAxis, &out->zAxis, &xAxis);
    SE_Vec3f_Normalize(&xAxis, &out->xAxis);
    SE_Vec3f_Copy(location, &out->location);
    SE_Frustum_InitFromFOV(fov, ratio, near, far, &out->frustum);
	out->changed = 1;
    return SE_VALID;
}

int SE_Camera_CullBoundingVolume(const SE_Camera* camera, SE_BoundingVolume* bv)
{
	SE_Plane cullPlanes[6];
	enum SE_Plane_Side planeSide = SE_NEGATIVE;
	int i, ret;
	SE_Camera_GetPlanes(camera, cullPlanes);
	for(i = 0 ; i < 6 ; i++)
	{
		enum SE_Plane_Side p = (*bv->fWhichSide)(bv, &cullPlanes[i]);
        if(p == SE_POSITIVE)
			return -1;
		if(p != SE_NEGATIVE)
		    planeSide = p;
	}
	if(planeSide == SE_NEGATIVE)
		return 1;
	else
		return 0;
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
    SE_Matrix3f initm;
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
	camera->changed = 1;
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
	camera->changed = 1;
    return SE_VALID;
}
SE_Result SE_Camera_ScreenCoordinateToRay(SE_Camera* camera, int x, int y, SE_Ray* out)
{
    SE_Recti viewport;
    SE_Rectf nearRect;
    SE_Frustum* frustum;
    SE_Vector3f dir;
    float xp, yp, xv, yv, dirLen;
    viewport = camera->viewport;
    frustum = &camera->frustum;
    xp = ((float)x) / (viewport.right - viewport.left);
    yp = 1 - ((float)y) / (viewport.bottom - viewport.top);
    SE_Frustum_GetNearPlaneRect(frustum, &nearRect);
    xv = (1 - xp) * nearRect.left + xp * nearRect.right;
    yv = (1 - yp) * nearRect.bottom + yp * nearRect.top;
    dirLen = SE_Sqrtf(xv * xv + yv * yv + frustum->n * frustum->n);
    /*
    SE_Vec3f_Mul(&camera->xAxis, xv / dirLen, &dirx);
    SE_Vec3f_Mul(&camera->yAxis, yv / dirLen, &diry);
    SE_Vec3f_Mul(&camera->zAxis, -frustum->n / dirLen, &dirz);
    SE_Vec3f_Add(&dirx , &diry, &tmp);
    SE_Vec3f_Add(&dirz, &tmp, &dir);
    */
    dir.x = camera->xAxis.x * xv / dirLen + camera->yAxis.x * yv / dirLen + camera->zAxis.x * (-frustum->n) / dirLen;
    dir.y = camera->xAxis.y * xv / dirLen + camera->yAxis.y * yv /dirLen + camera->zAxis.y * (-frustum->n) / dirLen;
    dir.z = camera->xAxis.z * xv / dirLen + camera->yAxis.z * yv / dirLen + camera->zAxis.z * (-frustum->n) / dirLen;
    SE_Ray_InitFromDirection(&camera->location, &dir, 0, out); 
    return SE_VALID;
}
SE_Result SE_Camera_SetFrustum(SE_Camera* camera , float fov, float ratio, float nearp, float farp)
{
    SE_Frustum frustum;
	SE_Result ret;
	SE_Object_Clear(&frustum, sizeof(SE_Frustum));
    ret = SE_Frustum_InitFromFOV(fov, ratio, nearp, farp, &frustum);
	if(ret == SE_VALID)
	{
		SE_Frustum_Copy(&frustum, &camera->frustum);
		camera->changed = 1;
		return SE_VALID;
	}
	return SE_INVALID;
}

SE_Result SE_Camera_GetPlanes(const SE_Camera* camera, SE_Plane* planes)
{
	SE_Vector3f NearLeftBottom, NearLeftTop, NearRightBottom, NearRightTop,
		        FarLeftBottom, FarLeftTop, FarRightBottom, FarRightTop;
	SE_Vector3f tmp1;
	SE_Vector3f tmp[4];
	SE_Frustum* frustum = &camera->frustum;
	SE_Rectf nearplane;
    SE_ASSERT(camera);
	SE_ASSERT(planes);
	SE_Frustum_GetNearPlaneRect(frustum, &nearplane);
	SE_Vec3f_Mul(&camera->zAxis, -frustum->n, &tmp[0]);
	SE_Vec3f_Mul(&camera->xAxis, nearplane.left, &tmp[1]);
	SE_Vec3f_Mul(&camera->yAxis, nearplane.bottom, &tmp[2]);
	SE_Vec3f_Copy(&camera->location, &tmp[3]);
	SE_Vec3f_AddSequence(tmp, 4, &NearLeftBottom);
    SE_Vec3f_AddSequence(tmp, 3, &tmp1);
    SE_Vec3f_Mul(&tmp1, frustum->f / frustum->n, &tmp1);
	SE_Vec3f_Add(&camera->location, &tmp1, &FarLeftBottom);

	SE_Vec3f_Mul(&camera->xAxis, nearplane.left, &tmp[1]);
	SE_Vec3f_Mul(&camera->yAxis, nearplane.top, &tmp[2]);
	SE_Vec3f_AddSequence(tmp, 4, &NearLeftTop);
	SE_Vec3f_AddSequence(tmp, 3, &tmp1);
    SE_Vec3f_Mul(&tmp1, frustum->f / frustum->n, &tmp1);
	SE_Vec3f_Add(&camera->location, &tmp1, &FarLeftTop);

	SE_Vec3f_Mul(&camera->xAxis, nearplane.right, &tmp[1]);
	SE_Vec3f_Mul(&camera->yAxis, nearplane.bottom, &tmp[2]);
	SE_Vec3f_AddSequence(tmp, 4, &NearRightBottom);
    SE_Vec3f_AddSequence(tmp, 3, &tmp1);
    SE_Vec3f_Mul(&tmp1, frustum->f / frustum->n, &tmp1);
	SE_Vec3f_Add(&camera->location, &tmp1, &FarRightBottom);

 	SE_Vec3f_Mul(&camera->xAxis, nearplane.right, &tmp[1]);
	SE_Vec3f_Mul(&camera->yAxis, nearplane.top, &tmp[2]);
	SE_Vec3f_AddSequence(tmp, 4, &NearRightTop);

	SE_Vec3f_AddSequence(tmp, 3, &tmp1);
    SE_Vec3f_Mul(&tmp1, frustum->f / frustum->n, &tmp1);
	SE_Vec3f_Add(&camera->location, &tmp1, &FarRightTop);
    
	SE_Plane_InitFromPoint(&NearLeftBottom, &camera->location, &NearLeftTop, &planes[0]);
	SE_Plane_InitFromPoint(&NearLeftBottom, &NearLeftTop, &camera->location, &planes[1]);

	SE_Plane_InitFromPoint(&camera->location, &NearRightBottom, &NearLeftBottom, &planes[2]);
	SE_Plane_InitFromPoint(&camera->location, &NearRightTop, &NearLeftTop, &planes[3]);

	SE_Plane_InitFromPoint(&NearLeftBottom, &NearRightBottom, &NearRightTop, &planes[4]);

	SE_Plane_InitFromPoint(&FarLeftBottom, &FarLeftTop, &FarRightTop, &planes[5]);
	return SE_VALID;


}