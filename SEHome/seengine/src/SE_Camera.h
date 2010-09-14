#ifndef SE_CAMERA_H
#define SE_CAMERA_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Geometry3D.h"
#include "SE_Common.h"
#include "SE_Object.h"
class SE_BoundingVolume;
class SE_Camera : public SE_Object
{
DECLARE_OBJECT(SE_Camera)
public:
    SE_Camera();
	~SE_Camera();
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    SE_Camera(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& up, float fov, float ratio, float near, float far);
    int cullBV(const SE_BoundingVolume& bv) const;
    void setViewport(int x, int y, int w, int h);
    SE_Rect<int> getViewport() const;
    SE_Matrix4f getWorldToViewMatrix() const;
    SE_Matrix4f getViewToWorldMatrix() const;
    void setFrustum(float fov, float ratio, float near, float far);
    void translateLocal(const SE_Vector3f& translate);
    SE_Ray screenCoordinateToRay(int x, int y);
    void getFrustumPlanes(SE_Plane planes[6]) const;
    void setLocation(const SE_Vector3f& loc);
	SE_Vector3f getLocation()
	{
		return mLocation;
	}
    //0: x axis, 1
    void rotateLocal(float angle, SE_AXIS_TYPE axis);
    void rotateLocal(const SE_Quat& rotate);
    void create(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far);
    void create(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& up, float fov, float ratio, float near, float far);
    SE_Matrix4f getPerspectiveMatrix() const;
	//camera will own bv, camera will delete mBoundingVolume which is assigned by bv;
	void setBoundingVolume(const SE_BoundingVolume* bv)
	{
		if(mBoundingVolume)
			delete mBoundingVolume;
		mBoundingVolume = bv;
	}
	const SE_BoundingVolume* getBoundingVolume() const
	{
		return mBoundingVolume;
	}
private:
    SE_Frustum mFrustum;
    SE_Rect<int> mViewport;
    SE_Vector3f mLocation;
    SE_Vector3f mAxisX;
    SE_Vector3f mAxisY;
    SE_Vector3f mAxisZ;
	mutable SE_Plane mPlanes[6];
    bool mChanged;
	const SE_BoundingVolume* mBoundingVolume;
};
#endif
