#include "SE_BoundingVolume.h"
#include "SE_Utils.h"

SE_BoundingVolume::SE_BoundingVolume()
{}
SE_BoundingVolume::~SE_BoundingVolume()
{}
void SE_BoundingVolume::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_BoundingVolume::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
/////////////////////////////////////////
SE_SphereBV::SE_SphereBV()
{}
SE_SphereBV::~SE_SphereBV()
{}
void SE_SphereBV::write(SE_BufferOutput& output) const
{}
void SE_SphereBV::read(SE_BufferInput& input)
{}
void SE_SphereBV::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_SphereBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
SE_Plane_Side SE_SphereBV::whichSide(const SE_Plane& plane) const
{
	return SE_NEGATIVE;
}
SE_IntersectResult SE_SphereBV::intersect(const SE_Ray& ray) const
{
	return SE_IntersectResult();
}
bool SE_SphereBV::intersect(const SE_BoundingVolume& bv) const
{
	return false;
}
int SE_SphereBV::movingSphereIntersect(const SE_Sphere& sphere, const SE_Vector3f& endPoint, SE_Vector3f* out)
{
	return 0;
}
int SE_SphereBV::movingOBBIntersect(const SE_OBB& obb, SE_AXIS_TYPE axis, float dist, SE_Vector3f* out)
{
	return 0;
}
SE_BoundingVolume::BV_TYPE SE_SphereBV::getType() const
{
	return SPHERE;
}
void SE_SphereBV::merge(const SE_BoundingVolume* bv)
{}
/////////////////////////////////////////////
SE_AABBBV::SE_AABBBV() : mAABB(SE_Vector3f(SE_FLT_MAX, SE_FLT_MAX, SE_FLT_MAX), 
							   SE_Vector3f(-SE_FLT_MAX, -SE_FLT_MAX, -SE_FLT_MAX))
{

}
SE_AABBBV::~SE_AABBBV()
{}
SE_AABBBV::SE_AABBBV(const SE_AABB& aabb)
{}
void SE_AABBBV::write(SE_BufferOutput& output) const
{}
void SE_AABBBV::read(SE_BufferInput& input)
{}
void SE_AABBBV::createFromPoints(SE_Vector3f* points, int num)
{
	mAABB.createFromPoints(points, num);
}
void SE_AABBBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
	
}
SE_Plane_Side SE_AABBBV::whichSide(const SE_Plane& plane) const
{
	return mAABB.whichSide(plane);
}
SE_IntersectResult SE_AABBBV::intersect(const SE_Ray& ray) const
{
	return mAABB.intersect(ray);
}
bool SE_AABBBV::intersect(const SE_BoundingVolume& bv) const
{
	return false;
}
SE_BoundingVolume::BV_TYPE SE_AABBBV::getType() const
{
	return AABB;
}
void SE_AABBBV::merge(const SE_BoundingVolume* bv)
{
	if(!bv || bv->getType() != AABB)
	{
		return;
	}
	SE_AABBBV* aabbBv = (SE_AABBBV*)bv;
	const SE_Vector3f& min1 = mAABB.getMin();
	const SE_Vector3f& max1 = mAABB.getMax();
	const SE_Vector3f& min2 = aabbBv->mAABB.getMin();
	const SE_Vector3f& max2 = aabbBv->mAABB.getMax();
	SE_Vector3f minf, maxf;
	for(int i = 0 ; i < 3 ; i++)
	{
		minf.d[i] = SE_Util::min(min1.d[i], min2.d[i]);
		maxf.d[i] = SE_Util::max(max1.d[i], max2.d[i]);
	}
	mAABB.set(minf, maxf);
}
int SE_AABBBV::movingSphereIntersect(const SE_Sphere& sphere, const SE_Vector3f& endPoint, SE_Vector3f* out)
{
	return SE_GeometryIntersect::movingSphereStaticAABB(sphere, mAABB, endPoint, out);
}
int SE_AABBBV::movingOBBIntersect(const SE_OBB& obb, SE_AXIS_TYPE axis, float dist, SE_Vector3f* out)
{
	return 0;
}
///////////////////////////////////////////
SE_OBBBV::SE_OBBBV()
{}
SE_OBBBV::SE_OBBBV(const SE_OBB& obb)
{}
SE_OBBBV::~SE_OBBBV()
{}
void SE_OBBBV::write(SE_BufferOutput& output) const
{}
void SE_OBBBV::read(SE_BufferInput& input)
{}
void SE_OBBBV::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_OBBBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
SE_Plane_Side SE_OBBBV::whichSide(const SE_Plane& plane) const
{
	return SE_NEGATIVE;
}
SE_IntersectResult SE_OBBBV::intersect(const SE_Ray& ray) const
{
	return SE_IntersectResult();
}
bool SE_OBBBV::intersect(const SE_BoundingVolume& bv) const
{
	return false;
}
SE_BoundingVolume::BV_TYPE SE_OBBBV::getType() const
{
	return OBB;
}
void SE_OBBBV::merge(const SE_BoundingVolume* bv)
{}
int SE_OBBBV::movingSphereIntersect(const SE_Sphere& sphere, const SE_Vector3f& endPoint, SE_Vector3f* out)
{
	return 0;
}
int SE_OBBBV::movingOBBIntersect(const SE_OBB& obb, SE_AXIS_TYPE axis, float dist, SE_Vector3f* out)
{
	return 0;
}


