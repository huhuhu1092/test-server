#ifndef SE_CAMERA_H
#define SE_CAMERA_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Geometry3D.h"
#include "SE_Common.h"
class SE_Camera
{
public:
    SE_Camera();
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& dir, const SE_Vector3f& up, float fov, float far, float near, float far);
    int cullBV(const SE_BoundingVolume& bv);
    void setViewport(int x, int y, int w, int h);
    SE_Rect<int> getViewport();
    SE_Matrix4f getWorldToViewMatrix();
    SE_Matrix4f getViewToWorldMatrix();
    void setFrustum(float fov, float ratio, float near, float far);
    void translateLocal(const SE_Vector3f& translate);
    SE_Ray screenCoordinateToRay(int x, int y);
    void getPlanes(SE_Plane planes[6]);
    void setLocation(const SE_Vector3f& loc);
    //0: x axis, 1
    void rotateLocal(float angle, SE_AXIS_TYPE axis);
    void rotateLocal(const SE_Quat& rotate);
    void create(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& dir, const SE_Vector3f& up, float fov, float far, float near, float far);
private:
    SE_Frustum mFrustum;
    SE_Rect<int> mViewport;
    SE_Vector3f mLocation;
    SE_Vector3f mAxisX;
    SE_Vector3f mAxisY;
    SE_Vector3f mAxisZ;
    bool mChanged;
};
#endif
