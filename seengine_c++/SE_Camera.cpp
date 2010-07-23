#include "SE_Camera.h"
SE_Camera::SE_Camera()
{
    mChanged = true;
}
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    SE_Vector3f zDir = location - target;
    if(zDir.isZero)
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
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far)
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
SE_Camera::SE_Camera(const SE_Vector3f& location, const SE_Vector3f& zAxis, const SE_Vector3f& yAxis, float fov, float ratio, float near, float far)
{
    mFrustum.set(fov, ratio, near, far);
    SE_Vector3f xAxis = yAxis.cross(zAxis);
    mAxisX = xAxis.normalize();
    mAxisY = zAxis.cross(xAxis).normalize();
    mAxisZ = zAxis.normalize();
    mLoacation = location;
    mChanged = true;
}
int SE_Camera::cullBV(const SE_BoundingVolume& bv)
{
    SE_Plane cullPlanes[6];
    SE_Plane_Side planeSide = SE_NEGATIVE;
    getFrustumPlanes(cullPlanes);
    for(int i = 0 ; i < 6 ; i++)
    {
        SE_Plane_Side p = bv.whichSide(cullPlanes[i]);
        if(p == SE_POSITIVE)
            return -1;
        if(p != SE_NEGATIVE)
            planeSide = p;
    }
    if(planeSize == SE_NAGETIVE)
        return 1;
    else
        return 0;
}
void SE_Camera::setViewport(int x, int y, int w, int h)
{
    mViewport.left = x;
    mViewport.right = x + w;
    mViewport.top = y;
    mViewport.bottom = y + h;
}
SE_Matrix4f SE_Camera::getWorldToViewMatrix()
{
    SE_Matrix4f vtow = getViewToWorld();
    return vtow.inserse(); 
}
SE_Matrix4f SE_Camera::getViewToWorldMatrix()
{
    SE_Matrix3f rm;
    rm.setColumn(0, xAxis);
    rm.setColumn(1, yAxis);
    rm.setColumn(2, zAxis);
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
SE_Ray SE_Camera::screenCoordinateToRay(int x, int y)
{
    float xp = ((float)x) / (mViewport.right - mViewport.left);
    float yp = 1 - ((float)y) / (mViewport.bottom - mViewport.top);
    SE_Rect<float> nearRect = mFrustum.getNearPlaneRect();
    float xv = (1 - xp) * nearRect.left + xp * nearRect.right;
    float yv = (1 - yp) * nearRect.bottom + yp * nearRect.top;
    float dirLen = SE_Sqrtf(xv * xv + yv * yv + mFrustum.getNear() * mFrustum.getNear());
    SE_Vector3f dir;
    dir.x = mAxis.x * xv / dirLen + mAxisY.x * yv / dirLen + mAxisZ.x * (-mFrustum.getNear()) / dirLen;
    dir.y = mAxisX.y * xv / dirLen + mAxisY.y * yv /dirLen + mAxisZ.y * (-mFrustum.getNear()) / dirLen;
    dir.z = mAxisX.z * xv / dirLen + mAxisY.z * yv / dirLen + mAxisZ.z * (-mFrustum.getNear()) / dirLen;
    return SE_Ray(mLocation, dir);
}
void SE_Camera::getFrustumPlanes(SE_Plane planes[6])
{
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
        planes[i] = lplanes[i].transform(vtom);
    }
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
    case 0:
        q.set(angle, SE_Vector3f(1, 0, 0));
        break;
    case 1:
        q.set(angle, SE_Vector3f(0, 1, 0));
        break;
    case 2:
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
    localxAxis = rotate.map(xAxis);
    localyAxis = rotate.map(yAxis);
    //localzAxis = rotate.map(zAxis);
    SE_Matrix4f vtom = getViewToWorldMatrix();
    SE_Vector4f worldxAxis = vtom.map(SE_Vector4f(localxAxis, 0));
    SE_Vector4f worldyAxis = vtom.map(SE_Vector4f(localyAxis, 0));
    //SE_Vector4f worldzAxis = vtom.map(SE_Vector4f(localzAxis, 0));
    SE_Vector4f worldzAxis = worldxAxis.cross(worldyAxis);
    mAxisX = worldxAxis.normalize();
    mAxisY = worldyAxis.normalize();
    mAxisZ = worldzAxis.normalize();
    mChanged = true;
}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& target, float fov, float ratio, float near, float far)
{}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, const SE_Vector3f& zAxis, float fov, float ratio, float near, float far)
{}
void SE_Camera::create(const SE_Vector3f& location, const SE_Vector3f& dir, const SE_Vector3f& up, float fov, float far, float near, float far)
{}
