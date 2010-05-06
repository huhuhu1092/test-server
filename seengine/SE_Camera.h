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
    SE_Recti viewport;
} SE_Camera;
extern SE_Result SE_Camera_InitByLocationTarget(const SE_Vector3f* location, const SE_Vector3f* target, float fov, float ratio, float near, float far, SE_Camera* out);
extern SE_Result SE_Camera_InitByFrame(const SE_Vector3f* location, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis, float fov, float ratio, float near, float far, SE_Camera* out);
extern SE_Result SE_Camera_InitByDirectionUp(const SE_Vector3f* location, const SE_Vector3f* dir, const SE_Vector3f* up, float fov, float ratio, float near, float far, SE_Camera* out);

// -1: cull fully
// 1: not cull
// 0: intersect partly
extern SE_Result SE_Camera_CullBouningVolume(const SE_Camera* camera, SE_BoundingVolume* bv);
extern SE_Result SE_Camera_Update(SE_Camera* camera);
extern SE_Result SE_Camera_SetViewport(SE_Camera* camera, int x, int y, int w, int h);
extern SE_Result SE_Camera_GetMatrixWorldToView(SE_Camera* camera, SE_Matrix4f* out);
extern SE_Result SE_Camera_GetMatrixViewToWorld(SE_Camera* camera, SE_Matrix4f* out);
/**
 * rotate about the local axis x, y, z.
 * 0: x axis
 * 1: y axis
 * 2: z axis
 * */
extern SE_Result SE_Camera_RotateLocalXYZAxis(SE_Camera* camera, float rotateAngle, int axis);
#ifdef __cplusplus
}
#endif
#endif
