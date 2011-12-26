#include "SE_Camera.h"
#include "SE_Log.h"

#include <math.h>
///////////////
SE_Camera::~SE_Camera()
{
	LOGI("### destroctor ~SE_Camera ####\n");
}
SE_Camera* SE_Camera::create2DSceneCamera(float width, float height)
{
	SE_Vector3f location(0, 0, 10);
    float ratio = height / (float)width;
	float angle = 2 * SE_RadianToAngle(atanf(width / 20.0f));
    SE_Camera* camera = new SE_Camera;
	camera->create(location, SE_Vector3f(0, 0, 1), SE_Vector3f(0, 1, 0), angle, ratio, 1, 50);//(location, SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), angle * 2, ratio, 1, 20);
	camera->setViewport(0, 0, (int)width, (int)height);
	return camera;
}
SE_Camera::SE_Camera()
{
    mChanged = true;
}
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far)
{
	create(location, target, fov, ratio, near, far);
}
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far)
{
	create(location, xAxis, yAxis, zAxis, fov, ratio, near, far);
}
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& yAxis, float fov, float ratio, float near, float far)
{
	create(location, zAxis, yAxis, fov, ratio, near, far);
}
SE_Rect<int> SE_Camera::getViewport() const
{
    return mViewport;
}
void SE_Camera::setViewport(int x, int y, int w, int h)
{
    mViewport.left = x;
    mViewport.right = x + w;
    mViewport.top = y;
    mViewport.bottom = y + h;
}
SE_Matrix4f SE_Camera::getWorldToViewMatrix() const
{
    SE_Matrix4f vtow = getViewToWorldMatrix();
    return vtow.inverse(); 
}
void SE_Camera::transformLocation(const SE_Matrix4f& m)
{
	SE_Vector4f newLocation = SE_Vector4f(mLocation, 1.0);
	mLocation = m.map(newLocation).xyz();
}
SE_Matrix4f SE_Camera::getViewToWorldMatrix() const
{
    SE_Matrix3f rm;
    rm.setColumn(0, mAxisX);
    rm.setColumn(1, mAxisY);
    rm.setColumn(2, mAxisZ);
    return SE_Matrix4f(rm, mLocation);
}
void SE_Camera::setFrustum(float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    mChanged = true;
}
void SE_Camera::translateLocal(const SE_Vector3f& translate)
{
    mLocation = mLocation + mAxisX * translate.x + mAxisY * translate.y + mAxisZ * translate.z;
    mChanged = true; 
}

void SE_Camera::getFrustumPlanes(SE_Plane planes[6]) const
{
	if(!mChanged)
	{
		for(int i = 0 ; i < 6 ; i++)
		{
			planes[i] = mPlanes[i];
		}
		return;
	}
    SE_Plane lplanes[6];
    lplanes[0] = mFrustum.getLeftPlane();
    lplanes[1] = mFrustum.getRightPlane();
    lplanes[2] = mFrustum.getTopPlane();
    lplanes[3] = mFrustum.getBottomPlane();
    lplanes[4] = mFrustum.getNearPlane();
    lplanes[5] = mFrustum.getFarPlane();
    SE_Matrix4f vtom = getViewToWorldMatrix();
    vtom = vtom.inverse();
    vtom = vtom.transpose();
    for(int i = 0 ; i < 6 ; i++)
    {
        SE_Plane p = lplanes[i].transform(vtom);
		planes[i].set(p.getNormal().neg(), p.getDistance());
		mPlanes[i] = planes[i];
    }
#ifdef DEBUG
	/*
	SE_Plane nplanes[6];
	SE_Vector3f NearLeftBottom, NearLeftTop, NearRightBottom, NearRightTop,
		        FarLeftBottom, FarLeftTop, FarRightBottom, FarRightTop;
	SE_Vector3f tmp1;
	SE_Vector3f tmp[4];
	SE_Rect<float> nearplane = mFrustum.getNearPlaneRect();
	tmp[0] = mAxisZ.mul(-mFrustum.getNear());
	tmp[1] = mAxisX.mul(nearplane.left);
	tmp[2] = mAxisY.mul(nearplane.bottom);
	tmp[3] = mLocation;
	NearLeftBottom = tmp[0] + tmp[1] + tmp[2] + tmp[3];
	tmp1 = tmp[0] + tmp[1] + tmp[2];
	tmp1 = tmp1.mul(mFrustum.getFar() / mFrustum.getNear());
	FarLeftBottom = mLocation + tmp1;

	tmp[1] = mAxisX.mul(nearplane.left);
	tmp[2] = mAxisY.mul(nearplane.top);
	NearLeftTop = tmp[0] + tmp[1] + tmp[2] + tmp[3];
    tmp1 = tmp[0] + tmp[1] + tmp[2];
	tmp1 = tmp1.mul(mFrustum.getFar() / mFrustum.getNear());
	FarLeftTop = mLocation + tmp1;

	tmp[1] = mAxisX.mul(nearplane.right);
	tmp[2] = mAxisY.mul(nearplane.bottom);
	NearRightBottom = tmp[0] + tmp[1] + tmp[2] + tmp[3];
    tmp1 = tmp[0] + tmp[1] + tmp[2];
	tmp1 = tmp1.mul(mFrustum.getFar() / mFrustum.getNear());
	FarRightBottom = mLocation + tmp1;

	tmp[1] = mAxisX.mul(nearplane.right);
	tmp[2] = mAxisY.mul(nearplane.top);
	NearRightTop = tmp[0] + tmp[1] + tmp[2] + tmp[3];

	tmp1 = tmp[0] + tmp[1] + tmp[2];
	tmp1 = tmp1.mul(mFrustum.getFar() / mFrustum.getNear());
	FarRightTop = mLocation + tmp1;

	nplanes[0].set(NearLeftBottom, mLocation, NearLeftTop);
	nplanes[1].set(NearRightTop, mLocation, NearRightBottom);
	nplanes[2].set(mLocation, NearLeftBottom, NearRightBottom);
	nplanes[3].set(mLocation, NearRightTop, NearLeftTop);
	nplanes[4].set(NearLeftBottom, NearRightBottom, NearRightTop);
	nplanes[5].set(FarLeftBottom, FarLeftTop, FarRightTop);
    for(int i = 0 ; i < 6 ; i++)
    {
        planes[i] = nplanes[i];
    }
	*/
#endif
}
void SE_Camera::setLocation(const SE_Vector3f& loc)
{
    mLocation = loc;
    mChanged = true;
}
void SE_Camera::rotateLocal(float angle, SE_AXIS_TYPE axis)
{
    SE_Quat q;
    switch(axis)
    {
    case SE_AXIS_X:
        q.set(angle, SE_Vector3f(1, 0, 0));
        break;
    case SE_AXIS_Y:
        q.set(angle, SE_Vector3f(0, 1, 0));
        break;
    case SE_AXIS_Z:
        q.set(angle, SE_Vector3f(0, 0, 1));
        break;
    }
    return rotateLocal(q);
}
void SE_Camera::rotateLocal(const SE_Quat& rotate)
{
    SE_Vector3f localxAxis(1, 0, 0);
    SE_Vector3f localyAxis(0, 1, 0);
    SE_Vector3f localzAxis(0, 0, 1);
    localxAxis = rotate.map(localxAxis);
    localyAxis = rotate.map(localyAxis);
    //localzAxis = rotate.map(zAxis);
    SE_Matrix4f vtom = getViewToWorldMatrix();
    SE_Vector4f worldxAxis = vtom.map(SE_Vector4f(localxAxis, 0));
    SE_Vector4f worldyAxis = vtom.map(SE_Vector4f(localyAxis, 0));
    //SE_Vector4f worldzAxis = vtom.map(SE_Vector4f(localzAxis, 0));
	SE_Vector4f worldzAxis(worldxAxis.xyz().cross(worldyAxis.xyz()), 0);
    mAxisX = worldxAxis.normalize().xyz();
    mAxisY = worldyAxis.normalize().xyz();
    mAxisZ = worldzAxis.normalize().xyz();
    mChanged = true;
}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    SE_Vector3f zDir = location - target;
    if(zDir.isZero())
    {
        LOGI("### camera direction is zero ####\n");
        zDir.set(0, 0, 1);
    }
    zDir = zDir.normalize();
    SE_Vector3f upDir(0, 1, 0);
    if(upDir == zDir)
    {
        upDir.set(0, 0, -1);
    }
    SE_Vector3f leftDir = upDir.cross(zDir);
    upDir = zDir.cross(leftDir);
    mAxisX = leftDir.normalize();
    mAxisY = upDir.normalize();
    mAxisZ = zDir;
    mLocation = location;
    mChanged = true;
    mViewport.left = 0;
    mViewport.right = 0;
    mViewport.top = 0;
    mViewport.bottom = 0;
}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    mLocation = location;
    mAxisX = xAxis;
    mAxisY = yAxis;
    mAxisZ = zAxis;
    mViewport.left = 0;
    mViewport.right = 0;
    mViewport.top = 0;
    mViewport.bottom = 0;
    mChanged = true;    
}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& up, float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    SE_Vector3f xAxis = up.cross(zAxis);
    mAxisX = xAxis.normalize();
    mAxisY = zAxis.cross(xAxis).normalize();
    mAxisZ = zAxis.normalize();
    mLocation = location;
    mChanged = true;
}
SE_Matrix4f SE_Camera::getPerspectiveMatrix() const
{
	return mFrustum.getPerspectiveMatrix();
}