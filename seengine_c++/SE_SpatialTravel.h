#ifndef SE_SPATIALTRAVEL_H
#define SE_SPATIALTRAVEL_H
#include "SE_Spatial.h"
#include "SE_Geometry3D.h"
#include "SE_Math.h"
#include <list>
class SE_SimObject;
class SE_FindSpatialCollision : public SE_SpatialTravel
{
public:
    SE_FindSpatialCollision(const SE_Ray& ray);
    ~SE_FindSpatialCollision();
    int visit(SE_Spatial* spatial);
	int visit(SE_SimObject* so);
	SE_SimObject* getCollisionObject();
	SE_Spatial* getCollisionSpatial();
private:
	struct CollisionResult
	{
		SE_Spatial* spatial;
		SE_SimObject* simObject;
		float distance;
		CollisionResult()
		{
			spatial = NULL;
			simObject = NULL;
			distance = SE_FLT_MAX;
		}
	};
	CollisionResult mResult;
	CollisionResult mMinResult;
	//std::list<CollisionResult> mCollisionResult;
    SE_Ray mRay;	
};
#endif
