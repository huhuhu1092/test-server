#ifndef SE_CAMERA_H
#define SE_CAMERA_H
#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_BoundingVolume.h"
#include "SE_Matrix.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_Camera_tag
{
    SE_Frustum frustum;
    SE_Vector3f xAxis;
    SE_Vector3f zAxis;
    SE_Vector3f yAxis;
    SE_Vector3f location;
	int changed;
    SE_Recti viewport;
} SE_Camera;
extern SE_Result SE_Camera_InitByLocationTarget(const SE_Vector3f* location, const SE_Vector3f* target, float fov, float ratio, float near, float far, SE_Camera* out);
extern SE_Result SE_Camera_InitByFrame(const SE_Vector3f* location, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis, float fov, float ratio, float near, float far, SE_Camera* out);
extern SE_Result SE_Camera_InitByDirectionUp(const SE_Vector3f* location, const SE_Vector3f* dir, const SE_Vector3f* up, float fov, float ratio, float near, float far, SE_Camera* out);

// -1: cull fully
// 1: not cull
// 0: intersect partly
extern int SE_Camera_CullBoundingVolume(const SE_Camera* camera, SE_BoundingVolume* bv);
extern SE_Result SE_Camera_Update(SE_Camera* camera);
extern SE_Result SE_Camera_SetViewport(SE_Camera* camera, int x, int y, int w, int h);
extern SE_Result SE_Camera_GetMatrixWorldToView(SE_Camera* camera, SE_Matrix4f* out);
extern SE_Result SE_Camera_GetMatrixViewToWorld(SE_Camera* camera, SE_Matrix4f* out);
/**
 * set the frustum of camera
 */
extern SE_Result SE_Camera_SetFrustum(SE_Camera* camera , float fov, float ratio, float nearp, float farp);
/**
 * rotate about the local axis x, y, z.
 * 0: x axis
 * 1: y axis
 * 2: z axis
 * */
extern SE_Result SE_Camera_RotateLocalXYZAxis(SE_Camera* camera, float rotateAngle, int axis);
/**
 * translate along x, y,z axis of camera
 * 0: x axis
 * 1: y axis
 * 2: z axis
 * */
extern SE_Result SE_Camera_LocationTranslateAlignXYZ(SE_Camera* camera, float translate, int axis);
/**
 * x, y is the Operation Window System coordinate, for example windows or Xserver
 * */
extern SE_Result SE_Camera_ScreenCoordinateToRay(SE_Camera* camera, int x, int y, SE_Ray* out);
/**
 * get the frustum plane in world coordinate space
 * planes is the array of SE_Plane, its size is six
 * 0 : left , 1: right, 2: bottom, 3: top, 4: near, 5: far
 */
extern SE_Result SE_Camera_GetPlanes(const SE_Camera* camera, SE_Plane* planes);
#ifdef __cplusplus
}
#endif
#endif
