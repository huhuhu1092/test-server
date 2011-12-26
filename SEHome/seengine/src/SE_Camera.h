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
	~SE_Camera();
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& up, float fov, float ratio, float near, float far);
    void setViewport(int x, int y, int w, int h);
    SE_Rect<int> getViewport() const;
    SE_Matrix4f getWorldToViewMatrix() const;
    SE_Matrix4f getViewToWorldMatrix() const;
    void setFrustum(float fov, float ratio, float near, float far);
    void translateLocal(const SE_Vector3f& translate);
	void transformLocation(const SE_Matrix4f& m);
    SE_Ray screenCoordinateToRay(int x, int y);
    void getFrustumPlanes(SE_Plane planes[6]) const;
    void setLocation(const SE_Vector3f& loc);
	//void setViewToWorldPrefixMatrix(const SE_Matrix4f& m);
	SE_Vector3f getLocation()
	{
		return mLocation;
	}
	SE_Vector3f getAxisX()
	{
		return mAxisX;
	}
	SE_Vector3f getAxisY()
	{
		return mAxisY;
	}
	SE_Vector3f getAxisZ()
	{
		return mAxisZ;
	}
    //0: x axis, 1
    void rotateLocal(float angle, SE_AXIS_TYPE axis);
    void rotateLocal(const SE_Quat& rotate);
    void create(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& up, float fov, float ratio, float near, float far);
    SE_Matrix4f getPerspectiveMatrix() const;
	static SE_Camera* create2DSceneCamera(float width, float height);
private:
    SE_Frustum mFrustum;
    SE_Rect<int> mViewport;
    SE_Vector3f mLocation;
    SE_Vector3f mAxisX;
    SE_Vector3f mAxisY;
    SE_Vector3f mAxisZ;
	mutable SE_Plane mPlanes[6];
    bool mChanged;
};
#endif
