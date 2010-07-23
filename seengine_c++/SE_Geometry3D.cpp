#include "SE_Geometry3D.h"
#include "SE_Math.h"
SE_Rect3D::SE_Rect3D()
{
    e[0] = e[1] = 0;
}
SE_Rect3D::SE_Rect3D(const SE_Vector3f& center, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, float e[2]) : mCenter(center)
{
    mAxis[0] = xAxis;
    mAxis[1] = yAxis;
    mExtent[0] = e[0];
    mExtent[1] = e[1];
}
SE_Vector3f SE_Rect3D::getCenter()
{
    return mCenter;
}
SE_Vector3f SE_Rect3D::getXAxis()
{
    return mAxis[0];
}
SE_Vector3f SE_Rect3D::getYAxis()
{
    return mAxis[1];
}
void SE_Rect3D::getExtent(float out[2])
{
    out[0] = mExtent[0];
    out[1] = mExtent[1];
}
void SE_Rect3D::getVertex(SE_Vector3f v[4])
{
    v[0] = mCenter - mAxis[0] * mExtent[0] - mAxis[1] * mExtent[1];
    v[1] = mCenter + mAxis[0] * mExtent[0] - mAxis[1] * mExtent[1];
    v[2] = mCenter + mAxis[0] * mExtent[0] + mAxis[1] * mExtent[1];
    v[3] = mCenter - mAxis[0] * mExtent[0] + mAxis[1] * mExtent[1];
}
/////////////////////

SE_Segment::SE_Segment()
{
    
}
SE_Segment::SE_Segment(const SE_Vector3f& start, const SE_Vector3f& end) : mStart(start), mEnd(end)
{
}
SE_Segment::SE_Segment(const SE_Vector3f& start, const SE_Vector3f& dir)
{
    mStart = start;
    mEnd = start + dir;
}
SE_Vector3f SE_Segment::getStart()
{
    return mStart;
}
SE_Vector3f SE_Segment::getEnd()
{
    return mEnd;
}
SE_Vector3f SE_Segment::getDirection()
{
    return mEnd - mStart;
}
///////////////

SE_Plane::SE_Plane(const SE_Vector3f& normal, float d) : mNormal(normal), mDistance(d)
{

}
SE_Plane::SE_Plane(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2)
{
    set(p0, p1, p2);
}
SE_Vector3f SE_Plane::getNormal()
{
    return mNormal;
}
void SE_Plane::set(const SE_Vector3f& normal, float d)
{
    mNormal = normal;
    mDistance = d;
}
SE_Plane SE_Plane::transform(const SE_Matrix4f& m)
{
    SE_Vector4f v(mNormal, -mDistance);
    SE_Vector4f out = m.map(v);
    return SE_Plane(out.xyz(), out.w);
}
void SE_Plane::set(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2)
{
    SE_Vector3f v0 = p1 - p0;
    SE_Vector3f v1 = p2 - p0;
    SE_Vector3f n = v0.cross(v1);
    if(n.isZero())
    {
        mDistance = 0;
        mNormal.setZero();
    }
    else
    {
        mNormal = n.normalize();
        mDistance = mNormal.dot(p0);
    }
}

float SE_Plane::getDistance()
{
    return mDistance;
}
SE_Plane_Side SE_Plane::whichSide(const SE_Vector3f& point)
{
    float ret = mNormal.dot(point) - mDistance;
    if(ret == 0.0f)
        return SE_OTHER;
    else if(ret > 0.0f)
        return SE_POSITIVE;
    else
        return SE_NEGATIVE;
}
float SE_Plane::distance(const SE_Vector3f& point)
{
    return SE_Fabs(mNormal.dot(point) - mDistance);
}
/////////////////////////
SE_Ray::SE_Ray()
{
}
SE_Ray::SE_Ray(const SE_Vector3f& start, const SE_Vector3f& end)
{
    mOrigin = start;
    SE_Vector3f dir = end - start;
    if(dir.isZero)
    {
        mDir.setZero();
    }
    else
        mDir = dit.normalize();
}
SE_Ray::SE_Ray(const SE_Vector3f& org, const SE_Vector3f& dir)
{
    mOrigin = org;
    mDir = dir.normalize();
}
SE_Vector3f SE_Ray::getOrigin()
{
    return mOrigin;
}
SE_Vector3f SE_Ray::getDirection()
{
    mDir;
}

///////////////////////
SE_Frustum::SE_Frustum()
{
    mFovAngle = mRatio = mNear = mFar = 0;
}
SE_Frustum::SE_Frustum(float fovAngle, float ratio, float n, float f)
{
    set(fovAngle, ratio, n, f);
}
void SE_Frustum::set(float fovAngle, float ratio, float n, float f)
{
    float fovRadian, e, param1, param2;
    fovRadian = SE_AngleToRadian(fovAngle);
    e = 1.0f / SE_Tanf(fovRadian / 2);
    param1 = 1.0f / SE_Sqrtf(e * e + 1);
    param2 = 1.0f / SE_Sqrtf(e * e + ratio * ratio);
    SE_Vector3f v(e / param1, 0, -1 / param1);
    mLeftp.set(v, 0);
    v.set(-e / param1, 0, -1 / param1);
    mRightp.set(v, 0);
    v.set(0, -e / param2, -ratio / param2);
    mTopp.set(v, 0);
    v.set(0, e / param2, -ratio / param2);
    mBottomp.set(v, 0);
    v.set(0,0, -1);
    mNearp.set(v, near);
    v.set(0, 0, 1);
    mFarp.set(v, -f);
    mNear = n;
    mFar = f;
    mFovAngle = fovAngle;
}
SE_Rect<float> SE_Frustum::getNearPlaneRect()
{
    SE_Rect<float> out;
	float fovRadian, e;
    fovRadian = SE_AngleToRadian(mFovAngle);
    e = 1.0 / SE_Tanf(mFovRadian / 2);
    out.left = - mNear / e;
    out.right = mNear / e;
    out.top = mNear * mRatio / e;
    out.bottom = - mNear * mRatio / e;
    return out;
}
SE_Matrix4f SE_Frustum::getPerspectiveMatrix()
{
    SE_Matrix4f out;
    SE_Rect<float> nearrect;
    float n, f;
    float l, r, b , t;
    nearrect = getNearPlaneRect();
    n = mNear;
    f = mFar;
    l = nearrect.left;
    r = nearrect.right;
    b = nearrect.bottom;
    t = nearrect.top;
    out->m00 = (2 * n) / (r - l);
    out->m01 = 0;
    out->m02 = (r + l) / (r - l);
    out->m03 = 0;
    out->m10 = 0;
    out->m11 = (2 * n) / (t - b);
    out->m12 = (t + b) / (t - b);
    out->m13 = 0;
    out->m20 = 0;
    out->m21 = 0;
    out->m22 = -(f + n) / (f - n);
    out->m23 = -(2 * n * f) / (f - n);
    out->m30 = 0;
    out->m31 = 0;
    out->m32 = -1;
    out->m33 = 0;
    return out;
}
SE_Plane SE_Frustum::getLeftPlane()
{
    return mLeftp
}
SE_Plane SE_Frustum::getRightPlane()
{
    return mRightp;
}
SE_Plane SE_Frustum::getTopPlane()
{
    return mTopp;
}
SE_Plane SE_Frustum::getBottomPlane()
{
    return mBottomp;
}
SE_Plane SE_Frustum::getFarPlane()
{
    return mFarp;
}
SE_Plane SE_Frustum::getNearPlane()
{
    return mNearp;
}
float SE_Frustum::getNear()
{
    return mNear;
}
float SE_Frustum::getFar()
{
    return mFar;
}
////////////////////////////////////////////
SE_Sphere::SE_Sphere()
{
    mRadius = 0;
}
SE_Sphere::SE_Sphere(const SE_Vector3f& center, float r) : mCenter(center), mRadius(r)
{}
void SE_Sphere::set(const SE_Vector3f& center, float r)
{
    mCenter = center;
    mRadius = r;
}
void SE_Sphere::mostSeparatedPointsOnAABB(int* min , int* max, SE_Vector3f* points, int numPoint)
{
    int minx = 0, miny = 0, minz = 0, maxx = 0, maxy = 0, maxz = 0;
    SE_Vector3f xAxisDir, yAxisDir, zAxisDir;
    int i;
	float dist2x, dist2y, dist2z;
    for(i = 1 ; i < numPoint ; i++)
    {
        if(points[i].x < points[minx].x)
            minx = i;
        if(points[i].y < points[miny].y)
            miny = i;
        if(points[i].z < points[minz].z)
            minz = i;
        if(points[i].x > points[maxx].x)
            maxx = i;
        if(points[i].y > points[maxy].y)
            maxy = i;
        if(points[i].z > points[maxz].z)
            maxz = i;
    }
    xAxisDir = points[maxx] - points[minx];
    yAxisDir = points[maxy] - points[miny];
    zAxisDir = points[maxz] - points[minz];
    dist2x = xAxisDir.lengthSquare();
    dist2y = yAxisDir.lengthSquare();
    dist2z = zAxisDir.lengthSquare();
    *min = minx;
    *max = maxx;
    if(dist2y > dist2x && dist2y > dist2z)
    {
        *max = maxy;
        *min = miny;
    }
    if(dist2z > dist2x && dist2z > dist2y)
    {
        *max = maxz;
        *min = minz;
    }

}
void SE_Sphere::sphereFromDistantPoints(SE_Vector3f* points, int pointNum)
{
    int min, max;
    SE_Vector3f minP;
    SE_Vector3f maxP;
    mostSeparatedPointsOnAABB(&min, &max, points, pointNum);
    minP = points[min];
    maxP = points[max];
    mCenter = (minP + maxP) * 0.5;
    mRadius = (points[max] - mCenter).length();
}
void SE_Sphere::sphereOfSphereAndPoint(SE_Vector3f* point)
{
    SE_Vector3f d, tmp1, tmp2;
	float dist2;
    d = point - mCenter;
    dist2 = d.lengthSquare();
    if(dist2 > mRadius * mRadius)
    {
        float dist = SE_Sqrtf(dist2);
        float newRadius = (mRadius + dist) * 0.5f;
        float k = (newRadius - mRadius) / dist;
        mRadius = newRadius;
        mCenter = d * k + mCenter;
    }
}

void SE_Sphere::ritterSphere(SE_Vector3f* points, int num)
{
    int i;
    sphereFromDistantPoints(points, pointNum);
    for(i = 0 ; i < pointNum ; i++)
    {
        sphereOfSphereAndPoint(&points[i]);
    }
    
}
void SE_Sphere::createFromPoints(SE_Vector3f* points, int num)
{
    ritterSphere(points, num);
}
SE_Vector3f SE_Sphere::getCenter()
{
    return mCenter;
}
float SE_Sphere::getRadius()
{
    return mRadius;
}
SE_IntersectResult SE_Sphere::intersect(const SE_AABB& aabb)
{}
SE_IntersectResult SE_Sphere::intersect(const SE_Ray& ray)
{}
SE_IntersectResult SE_Sphere::intersect(const SE_OBB& obb)
{

}
SE_Plane_Side SE_Sphere::whichSide(const SE_Plane& plane)
{}
SE_IntersectResult SE_Sphere::intersect(const SE_Sphere& sphere)
{

}
bool SE_Sphere::containPoint(const SE_Vector3f& point)
{
    SE_Vector3f d = mCenter - point;
    float dist2 = d.lengthSquare();
    if(dist2 > mRadius * mRadius)
    {
        return false;
    }
    else
        return true;
    
}
//////////////////////////////////////////////
SE_AABB::SE_AABB()
{}
SE_AABB::SE_AABB(const SE_Vector3f& min, const SE_Vector3f& max) : mMin(min), mMax(max)
{}
void SE_AABB::createFromPoints(SE_Vector3f* points, int num)
{
	int i;
    SE_Vector3f& min = points[0];
    SE_Vector3f& max = points[0];
    for(i = 1 ; i < pointNum ; i++)
    {
        SE_Vector3f& v = points[i];
        if(v.x < min.x)
            min.x = v.x;
        if(v.y < min.y)
            min.y = v.y;
        if(v.z < min.z)
            min.z = v.z;
        if(v.x > max.x)
            max.x = v.x;
        if(v.y > max.y)
            max.y = v.y;
        if(v.z > max.z)
            max.z = v.z;
    }
    mMin = min;
    mMax = max;
}
SE_Vector3f SE_AABB::getMin()
{
    return mMin;
}
SE_Vector3f SE_AABB::getMax()
{
    return mMax;
}
SE_Vector3f SE_AABB::getExtent()
{
    return mMax - mMin;
}
SE_Vector3f SE_AABB::getCenter()
{
    return mMin + (mMax - mMin) * 0.5;
}
SE_IntersectResult SE_AABB::intersect(const SE_AABB& aabb)
{
    SE_IntersectResult ret;
    if(mMax.x < aabb.min.x || mMin.x > aabb.mMax.x)
        return ret;
    if(mMax.y < aabb.mMin.y || mMin.y > aabb.mMax.y)
        return ret;
    if(mMax.z < aabb.mMin.z || mMin.z > aabb.mMax.z)
        return ret;
    ret.intersected = true;
    return ret;
    
}
SE_IntersectResult SE_AABB::intersect(const SE_OBB& obb)
{}
SE_IntersectResult SE_AABB::intersect(const SE_Sphere& sphere)
{}
SE_IntersectResult SE_AABB::intersect(const SE_Ray& ray)
{}
SE_Plane_Side SE_AABB::whichSide(const SE_Plane& plane)
{}

/////////////////////////////////////////////////
SE_OBB::SE_OBB()
{
    mExtent[0] = mExtent[1] = mExtent[2] = 0;
}
void SE_OBB::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_OBB::createFromAABB(const SE_AABB& aabb)
{
    mCenter = aabb.getCenter();
    SE_Vector3f extent = aabb.getExtent();
    mAxis[0].set(1, 0, 0);
    mAxis[1].set(0, 1, 0);
    mAxis[2].set(0, 0, 1);
    mExtent[0] = extent.x / 2;
    mExtent[1] = extent.y / 2;
    mExtent[2] = extent.z / 2;
}
void SE_OBB::getBoxVertex(SE_Vector3f out[8])
{}
SE_Vector3f SE_OBB::getCenter()
{
    return mCenter;
}
void SE_OBB::getAxis(SE_Vector3f axis[3])
{
    axis[0] = mAxis[0];
    axis[1] = mAxis[1];
    axis[2] = mAxis[2];
}
void SE_OBB::getExtent(float e[3])
{
    e[0] = mExtent[0];
    e[1] = mExtent[1];
    e[2] = mExtent[2];
}
void SE_OBB::transfrom(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
    mCenter = mCenter + translate;
    for(int i = 0 ; i < 3 ; i++)
    {
        mExtent[i] *= scale.d[i];
    }
    SE_Vector3f xAxis = rotate.map(mAxis[0]);
    SE_Vector3f yAxis = rotate.map(mAxis[1]);
    SE_Vector3f zAxis = rotate.map(mAxis[2]);
    xAxis = xAxis.normalize();
    yAxis = yAxis.normalize();
    zAxis = zAxis.normalize();
#ifdef DEBUG
    if(xAxis.cross(yAxis) != zAxis)
    {
        LOGI("### rotate can not equal\n");
    }
#endif 
    zAxis = xAxis.cross(yAxis);
    zAxis = zAxis.normalize();
    mAxis[0] = xAxis;
    mAxis[1] = yAxis;
    mAxis[2] = zAxis;
}
SE_IntersectResult SE_OBB::intersect(const SE_AABB& aabb)
{}
SE_IntersectResult SE_OBB::intersect(const SE_OBB& obb)
{
    float ra, rb;
    float x, y, z;
    SE_Matrix3f R, AbsR;
    int i , j;
    SE_Vector3f centerDistV, t;
    SE_Vector3f obbAxis[3];
    float obbExtent[3];
    obb.getAxis(obbAxis);
    obb.getExtent(obbExtent);
    if(a == NULL || b == NULL)
        return 0;
    for(i = 0 ; i < 3 ; i++)
    {
        for(j = 0 ; j < 3 ; j++)
        {
            R.set(i, j,  mAxis[i].dot(obbAxis[i]));
        }
    }
    centerDistV = obb.getCenter() - mCenter;
    x = centerDistV.dot(mAxis[0]);
    y = centerDistV.dot(mAxis[1]);
    z = centerDistV.dot(mAxis[2]);
    t.set(x, y, z);
    for(i = 0 ; i < 3 ; i++)
    {
        for(j = 0 ; j < 3 ; j++)
        {
            AbsR.set(i, j,  SE_Fabs(R(i , j) + SE_FLOAT_EQUAL_EPSILON));
        }
    }
    /*test axes L = A0, L = A1, L = A2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = mExtent[i];
        rb = obbExtent[0] * AbsR(i, 0) + obbExtent[1] * AbsR(i , 1) + obbExtent[2] * AbsR(i , 2);
        if(SE_Fabs(t.d[i]) > (ra + rb))
            return 0;
    }
    /*test axes L = B0, L = B1, L = B2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = mExtent[0] * AbsR(0, i) + mExtent[1] * AbsR(1 , i) + mExtent[2] * AbsR(2 , i);
        rb = obbExtent[i];
        if(SE_Fabs(t.d[0] * R(0, i) + t.d[1] * R(1 , i) + t.d[2] * R(2 ,i)) > ra + rb)
            return 0;
    }
    /*test axis L = A0 * B0 */
    ra = mExtent[1] * AbsR(2, 0) + mExtent[2] * AbsR(1, 0);
    rb = obbExtent[1] * AbsR(0, 2) + obbExtent[2] * AbsR(0, 1);
    if(SE_Fabs(t.d[2] * R(1, 0) - t.d[1] * R(2, 0)) > (ra + rb))
        return 0;
    /*test axis L = A0 * B1 */
    ra = mExtent[1] * AbsR(2, 1) + mExtent[2] * AbsR(1 ,  1);
    rb = obbExtent[0] * AbsR(0, 2) + obbExtent[2] * AbsR(0, 0);
    if(SE_Fabs(t.d[2] * R(1 , 1) - t.d[1] * R(2 ,  1)) > (ra + rb))
        return 0;
    /*test axis L = A0 * A2 */
    ra = mExtent[1] * AbsR(2 ,  2) + mExtent[2] * AbsR(1 ,  2);
    rb = obbExtent[0] * AbsR(0, 1) + obbExtent[1] * AbsR(0, 0);
    if(SE_Fabs(t.d[2] * R(1 ,  2) - t.d[1] * R(2 ,  2)) > (ra + rb))
        return 0;
    /*tes axis L = A1 * B0*/
    ra = mExtent[0] * AbsR(2 , 0) + mExtent[2] * AbsR(0 , 0);
    rb = obbExtent[1] * AbsR(1 ,  2) + obbExtent[2] * AbsR(1 , 1);
    if(SE_Fabs(t.d[0] * R(2, 0) - t.d[2] * R(0, 0)) > (ra + rb))
        return 0;
    /* test axis L = A1 * B1 */
    ra = mExtent[0] * AbsR(2 ,  1) + mExtent[2] * AbsR(0, 1);
    rb = obbExtent[0] * AbsR(1 ,  2) + obbExtent[2] * AbsR(1, 0);
    if(SE_Fabs(t.d[0] * R(2 , 1) - t.d[2] * R(0, 1)) > (ra + rb))
        return 0;
    
    /* test axis A1 * B2 */
    ra = mExtent[0] * AbsR(2 , 2) + mExtent[2] * AbsR(0, 2);
    rb = obbExtent[0] * AbsR(1 ,  1) + obbExtent[1] * AbsR(1 , 0);
    if(SE_Fabs(t.d[0] * R(2 , 2) - t.d[2] * R(0, 2)) > (ra + rb))
        return 0;

    /* test A2 * B0 */
    ra = mExtent[0] * AbsR(1 , 0) + mExtent[1] * AbsR(0, 0);
    rb = obbExtent[1] * AbsR(2 ,  2) + obbExtent[2] * AbsR(2 , 1);
    if(SE_Fabs(t.d[1] * R(0, 0) - t.d[0] * R(1 , 0)) > (ra + rb))
        return 0;

    /*test A2 * B1 */
    ra = mExtent[0] * AbsR(1 ,  1) + mExtent[1] * AbsR(0, 1);
    rb = obbExtent[0] * AbsR(2 ,  2) + obbExtent[2] * AbsR(2 , 0);
    if(SE_Fabs(t.d[1] * R(0, 1) - t.d[0] * R(1 ,  1)) > (ra + rb))
        return 0;
    
    /*test A2 * B2 */
    ra = mExtent[0] * AbsR(1 , 2) + mExtent[1] * AbsR(0, 2);
    rb = obbExtent[0] * AbsR(2 ,  1) + obbExtent[1] * AbsR(2 , 0);
    if(SE_Fabs(t.d[1] * R(0, 2) - t.d[0] * R(1 ,  2)) > (ra + rb))
        return 0;

    return 1;


}
SE_IntersectResult SE_OBB::intersect(const SE_Sphere& sphere)
{}
SE_IntersectResult SE_OBB::intersect(const SE_Ray& ray)
{}
SE_Plane_Side SE_OBB::whichSide(const SE_Plane& plane)
{}




