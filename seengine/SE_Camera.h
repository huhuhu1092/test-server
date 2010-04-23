#ifndef SE_CAMERA_H
#define SE_CAMERA_H
#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_BoundingVolume.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_Camera_tag
{
    SE_Frustum frustum;
    SE_Vector3f left;
    SE_Vector3f dir;
    SE_Vector3f up;
    SE_Vector3f location;
    SE_Recti viewport;
} SE_Camera;
#define SE_Camera_GetFrustum(camera) (&camera->frustum)
#define SE_Camera_GetLeftV(camera) (&camera->left)
#define SE_Camera_GetDirectionV(camera) (&camera->left)
#define SE_Camera_GetUpV(camera) (&camera->up)
#define SE_Camera_GetLocation(camera) (&camera->location)
#define SE_Camera_GetViewport(camera) (&camera->viewport)
#define SE_Camera_Clear(camera) (memset(camera, 0, sizeof(SE_Camera)))
extern SE_Result SE_Camera_InitByLocationTarget(const SE_Vector3f* location, const SE_Vector3f* target, float fov, float ratio, float near, float far);
extern SE_Result SE_Camera_InitByFrame(const SE_Vector3f* location, const SE_Vector3f* left, const SE_Vector3f* up, const SE_Vector3f* dir, float fov, float ratio, float near, float far);
extern SE_Result SE_Camera_InitByDirectionUp(const SE_Vector3f* location, const SE_Vector3f* dir, const SE_Vector3f* up, float fov, float ratio, float near, float far);

// -1: cull fully
// 1: not cull
// 0: intersect partly
extern SE_Result SE_Camera_CullBouningVolume(const SE_Camera* camera, SE_BoundingVolume* bv);
#ifdef __cplusplus
}
#endif
#endif
