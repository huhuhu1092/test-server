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
