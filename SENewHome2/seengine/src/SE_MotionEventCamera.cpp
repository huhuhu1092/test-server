#include "SE_MotionEventCamera.h"
#include "SE_SceneManager.h"
#include "SE_Application.h"
#include "SE_Geometry3D.h"
#include "SE_Spatial.h"
#include "SE_SpatialTravel.h"
#include "SE_SimObject.h"
#include "SE_Log.h"
#include "SE_Message.h"
#include "SE_MessageDefine.h"
#include "SE_ResourceManager.h"
#include "SE_Bone.h"
#include "SE_BipedAnimation.h"
#include "SE_SkinJointController.h"
#include "SE_BipedController.h"
#include "SE_SimObjectManager.h"
#include "SE_AnimationManager.h"
//#include "SE_TableManager.h"
#include "SE_Mesh.h"
#include "SE_UserCommand.h"
IMPLEMENT_OBJECT(SE_MotionEventCamera)


static void flushScreen()
{
    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);
    
}

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
        flushScreen();
	}
	else if((mPrevType == SE_MotionEvent::DOWN || mPrevType == SE_MotionEvent::MOVE)&& 
		    motionEvent->getType() == SE_MotionEvent::DOWN)
	{

        float deltaX = motionEvent->getX() - mPrevX;
		float deltaY = motionEvent->getY() - mPrevY;
	    LOGI("## motion event x = %f, y = %f ##\n", motionEvent->getX(), motionEvent->getY());
		LOGI("## prev x = %f, y = %f ##\n", mPrevX, mPrevY);
		LOGI("## delta x = %f, y = %f ##\n", deltaX, deltaY);
		if(mPrevType == SE_MotionEvent::DOWN && 
			(SE_Fabs(deltaX) > SE_MotionEvent::MOVE_SLOPE || SE_Fabs(deltaY) > SE_MotionEvent::MOVE_SLOPE))
		{
			mPrevType = SE_MotionEvent::MOVE;
		}
		if(mPrevType == SE_MotionEvent::MOVE)
		{
            SE_Rect<int> viewport = getViewport();
			int viewportWidth = viewport.right - viewport.left;
			LOGI("## viewportwidth = %d ##\n", viewportWidth);
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
			SE_Spatial* rootScene = sceneManager->getTopScene()->getRoot();
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
#if 0

        SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		//SE_SkinJointController* skinJointController = resourceManager->getSkinJointController("objLoft07");
        //SE_SkinJointController* skinJointController = resourceManager->getSkinJointController("man_head");
        SE_SkeletonController *sk = resourceManager->getSkeletonController(SE_SKELETONCONTROLLER);

        if(sk)
        {
            int bipedControllerNum = sk->mSkeletonController.size();
            for(int c = 0; c < bipedControllerNum; ++c)
            {
                SE_BipedController *bipedController = sk->mSkeletonController[c];
                if(bipedController)
                {

                    int animNum = bipedController->bipAndObjInfo.size();

                    for(int i = 0; i < animNum; ++i)
                    {


                        SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		            SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
		                //SE_SimObject* simobj = simObjectManager->findByName("objLoft07");
                            SE_SimObject* simobj = simObjectManager->findByName(bipedController->bipAndObjInfo[i]->objHasBiped.c_str());

	                    for(int j = 0; j < simobj->getSurfaceNum(); ++j)
                            {
                                simobj->getMesh()->getSurface(j)->setProgramDataID("skeletalanimation_shader");        
                                simobj->getMesh()->getSurface(j)->setRendererID("skeletalanimation_renderer");
			    
                            }

		            SE_Spatial* spatial = simobj->getSpatial();

                        SE_BipedAnimation* anim = (SE_BipedAnimation*)animManager->getAnimation(spatial->getAnimationID());

                        if(anim)
                        {
                            if(anim->getAnimState() == SE_Animation::RUNNING)
                            {
                                continue;
                            }
                        }
                        
                        anim = new SE_BipedAnimation();
                    
                        
		            anim->setRunMode(SE_Animation::NOT_REPEAT);
		            anim->setTimeMode(SE_Animation::SIMULATE);
		            anim->setDuration(1000);
		            anim->setSimObject(simobj);		            
                            anim->setSkinJointController(bipedController);

                            //set play mode with SHADER
                            anim->setPlayMode(SE_Animation::GPU_SKELETON_SHADER);
                            //anim->setPlayMode(SE_Animation::CPU_NO_SHADER);

		                
		            animManager->removeAnimation(spatial->getAnimationID());
		            SE_AnimationID animID = animManager->addAnimation(anim);
		            spatial->setAnimationID(animID);
		            anim->run();
                    }
                }
            }
        }
        else
        {
            SE_AddNewCbfCommand* c1 = (SE_AddNewCbfCommand*)SE_Application::getInstance()->createCommand("SE_AddNewCbfCommand");

            //set rotate angle per ticket
#ifndef ANDROID
            c1->dataPath = "e:\\model\\newhome3";
#else
            c1->dataPath = "/sdcard/sedemo/";
#endif

            //set spatialid. this is minute hand spatial
            c1->mCbfFileNameID = "man";            

            //post this command to command queue
            SE_Application::getInstance()->postCommand(c1);
        }

flushScreen();

#if 0
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

        if(so)
        {
            LOGI("## selected object = %s ####\n", so->getName());
			SE_Message* msg = new SE_Message;
			msg->type = SE_MSG_SIMOBJECT_NAME;
			SE_Struct* sestruct = new SE_Struct(1);
			SE_StructItem* sitem = new SE_StructItem(1);
			SE_StdString* stdString = new SE_StdString;
			stdString->data = so->getName();
			sitem->setDataItem(stdString);
			sestruct->setStructItem(0, sitem);
			msg->data = sestruct;
			SE_Application::getInstance()->sendMessage(msg);
        }
#endif
#endif
        clearState();
	}
}
