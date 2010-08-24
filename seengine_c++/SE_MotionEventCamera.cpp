#include "SE_MotionEventCamera.h"
#include "SE_SceneManager.h"
#include "SE_Application.h"
#include "SE_Geometry3D.h"
#include "SE_Spatial.h"
#include "SE_SpatialTravel.h"
#include "SE_SimObject.h"
#include "SE_Log.h"
IMPLEMENT_OBJECT(SE_MotionEventCamera)
SE_MotionEventCamera::SE_MotionEventCamera()
{
	mPrevType = SE_MotionEvent::UP;
	mPrevX = 0;
	mPrevY = 0;
}
SE_MotionEventCamera::~SE_MotionEventCamera()
{
	LOGI("### destroctor ~SE_MotionEventCamera ####\n");
}
void SE_MotionEventCamera::clearState()
{
	mPrevType = SE_MotionEvent::UP;
	mPrevX = 0;
	mPrevY = 0;
}
void SE_MotionEventCamera::onMotionEvent(SE_MotionEvent* motionEvent)
{
	if(motionEvent == NULL)
		return;
	if(mPrevType == SE_MotionEvent::UP && motionEvent->getType() == SE_MotionEvent::DOWN)
	{
		mPrevType = SE_MotionEvent::DOWN;
		mPrevX = motionEvent->getX();
		mPrevY = motionEvent->getY();
	}
	else if((mPrevType == SE_MotionEvent::DOWN || mPrevType == SE_MotionEvent::MOVE)&& 
		    motionEvent->getType() == SE_MotionEvent::DOWN)
	{
        float deltaX = motionEvent->getX() - mPrevX;
		float deltaY = motionEvent->getY() - mPrevY;
		if(mPrevType == SE_MotionEvent::DOWN && 
			(SE_Fabs(deltaX) > SE_MotionEvent::MOVE_SLOPE || SE_Fabs(deltaY) > SE_MotionEvent::MOVE_SLOPE))
		{
			mPrevType = SE_MotionEvent::MOVE;
		}
		if(mPrevType == SE_MotionEvent::MOVE)
		{
            SE_Rect<int> viewport = getViewport();
			int viewportWidth = viewport.right - viewport.left;
			float ratio = -180.0f / viewportWidth;
			float angle = ratio * deltaX;
			if(SE_Fabs(deltaY) > 9)
			{
				LOGI("#####\n");
			}
			rotateLocal(angle, SE_AXIS_Y);
			SE_Vector3f startLocation = getLocation();
			SE_Vector3f translate(0, 0, deltaY);
			translateLocal(translate);
			SE_Vector3f endLocation = getLocation();
            SE_Sphere sphere;
			sphere.set(startLocation, 2);
			SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
			SE_Spatial* rootScene = sceneManager->getRoot();
			SE_MovingSphereStaticSpatialIntersect moveTravel(sphere, endLocation);
			rootScene->travel(&moveTravel, true);
			if(moveTravel.intersected)
			{
				setLocation(moveTravel.location);
			}
			LOGI("### deltax = %f, deltay = %f, startLocation = (%f, %f, %f),  \
				 endLocation = (%f, %f, %f) ## \n" , deltaX, deltaY, startLocation.x, 
				                                   startLocation.y, startLocation.z,
												   endLocation.x, endLocation.y, endLocation.z);
			mPrevX = motionEvent->getX();
			mPrevY = motionEvent->getY();
		}
 	}
	else if(motionEvent->getType() == SE_MotionEvent::UP && mPrevType == SE_MotionEvent::MOVE)
	{
		clearState();
	}
	else if(motionEvent->getType() == SE_MotionEvent::UP && mPrevType == SE_MotionEvent::DOWN)
	{
		SE_Ray ray = screenCoordinateToRay(mPrevX, mPrevY);
        SE_FindSpatialCollision spatialCollision(ray);
		SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
		root->travel(&spatialCollision, true);
		SE_Spatial* collisionSpatial = spatialCollision.getCollisionSpatial();
		SE_SimObject* so = spatialCollision.getCollisionObject();
		if(collisionSpatial)
		{
            SE_Application::getInstance()->getSceneManager()->setSelectedSpatial(collisionSpatial);
			//collisionSpatial->setSelected(true);
		}
        clearState();
	}
}