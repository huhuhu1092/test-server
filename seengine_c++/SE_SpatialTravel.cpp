#include "SE_SpatialTravel.h"
#include "SE_SimObject.h"
#include "SE_BoundingVolume.h"
SE_FindSpatialCollision::SE_FindSpatialCollision(const SE_Ray& ray): mRay(ray)
{
	
}
SE_FindSpatialCollision::~SE_FindSpatialCollision()
{}
int SE_FindSpatialCollision::visit(SE_Spatial* spatial)
{
	SE_BoundingVolume* bv = spatial->getWorldBoundingVolume();
	SE_IntersectResult ret = bv->intersect(mRay);
	if(ret.intersected)
	{
		mResult.spatial = spatial;
		return 0;
	}
	else 
		return 1;
}
int SE_FindSpatialCollision::visit(SE_SimObject* so)
{
	int faceNum = so->getFaceNum();
	SE_Vector3i* faces = so->getFaceArray();
	SE_Vector3f* vertex = so->getVertexArray();
	float distance = SE_FLT_MAX;
	bool intersected = 0;
	for(int i = 0 ; i < faceNum ; i++)
	{
		SE_Triangle triangle(vertex[faces[i].x], vertex[faces[i].y], vertex[faces[i].z]);
		SE_IntersectResult ret = triangle.intersect(mRay);
		if(ret.intersected && ret.distance[0] < distance)
		{
			distance = ret.distance[0];
			intersected = 1;
		}
	}
	if(intersected)
	{
		mResult.simObject = so;
		mResult.distance = distance;
		if(mResult.distance < mMinResult.distance)
		{
			mMinResult = mResult;
		}
	}
	return 0;
}
SE_SimObject* SE_FindSpatialCollision::getCollisionObject()
{
	return mMinResult.simObject;
}
SE_Spatial* SE_FindSpatialCollision::getCollisionSpatial()
{
	return mMinResult.spatial;
}
SE_MovingSphereStaticSpatialIntersect::SE_MovingSphereStaticSpatialIntersect(const SE_Sphere& sphere, const SE_Vector3f& endPoint)
{
	intersected = false;
	this->sphere = sphere;
	this->endPoint = endPoint;
	location.set(endPoint.x, endPoint.y, endPoint.z);
}
int SE_MovingSphereStaticSpatialIntersect::visit(SE_Spatial* spatial)
{
    SE_Vector3f intersectPoint;
	if(!spatial->canDoCollision())
		return 1;
	SE_BoundingVolume* bv = spatial->getWorldBoundingVolume();
	if(!bv)
        return 0;
	int ret = bv->movingSphereIntersect(sphere, endPoint, &intersectPoint);
	if(ret)
	{
		SE_Vector3f center = sphere.getCenter();
		SE_Vector3f dir = (endPoint - center).normalize();
		float minDist = (location - center).dot(dir);
		float dist = (intersectPoint - center).dot(dir);
		if(minDist < dist)
		{
			location = intersectPoint;
		}
		return 0;
	}
	else
		return 1;
}
int SE_MovingSphereStaticSpatialIntersect::visit(SE_SimObject* so)
{
	return 0;
}

SE_MovingSpatialIntersect::SE_MovingSpatialIntersect(SE_Spatial* s)
{
	moveSpatial = s;
}

int SE_MovingSpatialIntersect::visit(SE_Spatial* spatial)
{
	SE_BoundingVolume* bv = spatial->getWorldBoundingVolume();
	if(!bv)
	{
		return 0;
	}
	SE_BoundingVolume* moveBV = moveSpatial->getWorldBoundingVolume();
	if(!moveBV)
	{
		return 1;
	}
	SE_OBB obb;
	SE_AABB aabb;
	SE_Sphere sphere;
	switch(moveBV->getType())
	{
	case SE_BoundingVolume::AABB:
		aabb = ((SE_AABBBV*)moveBV)->getGeometry();
		obb.createFromAABB(aabb);

		break;
	case SE_BoundingVolume::OBB:
		obb = ((SE_OBBBV*)moveBV)->getGeometry();
		break;
	case SE_BoundingVolume::SPHERE:
		sphere = ((SE_SphereBV*)moveBV)->getGeometry();
		break;
	}
}
int SE_MovingSpatialIntersect::visit(SE_SimObject* so)
{}