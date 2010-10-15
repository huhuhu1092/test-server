#include "PVRShell.h"
#include <stdio.h>
#include <math.h>
#include "SE_Ase.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_SystemCommand.h"
#include "SE_SystemCommandFactory.h"
#include "SE_InputEvent.h"
#include "SE_Primitive.h"
#include "SE_Geometry.h"
#include "SE_MeshSimObject.h"
#include "SE_SceneManager.h"
#include "SE_ImageCodec.h"
#include "SE_CommonNode.h"
#include "SE_Physics.h"
#include "SE_2DCommand.h"
#include "SE_Bone.h"
#include "SE_BoneAnimation.h"
#include "SE_SkinJointController.h"
#include "SE_SimObjectManager.h"
#include "SE_AnimationManager.h"
#include <ctype.h>
#include <stdarg.h>
#ifdef WIN32
#else
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#define SCREEN_WIDTH  480
#define SCREEN_HEIGHT 800
static void drawScene(int width, int height)
{
}

class SEDemo : public PVRShell
{
public:
	SEDemo()
	{
		mPhysics = NULL;
	}
	virtual bool InitApplication();
	virtual bool InitView();
	virtual bool ReleaseView();
	virtual bool QuitApplication();
	virtual bool RenderScene();
private:
	void handleInput(int width, int height);
private:
	SE_Physics* mPhysics;
	EyeData eyeData;
};
bool SEDemo::InitApplication()
{
	//PVRShellSet(prefWidth, SCREEN_WIDTH);
	//PVRShellSet(prefHeight, SCREEN_HEIGHT);
	SE_Application::SE_APPID appid;
	appid.first = 137;
	appid.second = 18215879;
	SE_Application::getInstance()->setAppID(appid);
	SE_SystemCommandFactory* sf = new SE_SystemCommandFactory;
	SE_Application::getInstance()->registerCommandFactory("SystemCommand", sf);
	//SE_Init2D* c = new SE_Init2D(SE_Application::getInstance());
    //c->data = &eyeData;
	SE_InitAppCommand* c = (SE_InitAppCommand*)SE_Application::getInstance()->createCommand("SE_InitAppCommand");
#ifdef WIN32
	c->dataPath = "c:\\model\\newhome3";//"D:\\model\\jme\\home\\newhome3";
#else
	c->dataPath = "/home/luwei/model/jme/home/newhome3";
#endif
	c->fileName = "home";
	SE_Application::getInstance()->postCommand(c);
	return true;
}
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	LOGI("## width = %d, height = %d ###\n", dwCurrentWidth,dwCurrentHeight);
	SE_UpdateCameraCommand* c = (SE_UpdateCameraCommand*)SE_Application::getInstance()->createCommand("SE_UpdateCameraCommand");
	c->width = dwCurrentWidth;
	c->height = dwCurrentHeight;
	SE_Application::getInstance()->postCommand(c);
	return true;
}
bool SEDemo::ReleaseView()
{
	
	return true;
}
bool SEDemo::QuitApplication()
{
	return true;
}
static SE_Vector3f startPos;
static SE_CommonNode* groupNode = NULL;
void SEDemo::handleInput(int width, int height)
{
    static float prevPointer[2];
    static bool bPressed = false;
    int buttonState = PVRShellGet(prefButtonState);
    float* pointerLocation = (float*)PVRShellGet(prefPointerLocation);
    /*LOGI("## buttonstate = %d ##\n", buttonState);*/
    if(pointerLocation)
    {
		LOGI("### pointer location = %f, %f ###\n", pointerLocation[0], pointerLocation[1]);
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
		SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::DOWN, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
	    bPressed = 1;
    }
    else if(bPressed)
    {
        SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::UP, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
        bPressed = 0;
    }
    if(PVRShellIsKeyPressed(PVRShellKeyNameLEFT))
    {
#if defined(WIN32)
		PCWSTR filePath = L"C:\\model\\test\\我的文件.jpg";
		SE_ImageData* imgd = SE_ImageCodec::load(filePath);
#endif
		float e[2] = {1, 1};
		SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), e);
		SE_RectPrimitive* primitive = NULL;
		SE_PrimitiveID primitiveID;
		SE_RectPrimitive::create(rect3D, primitive, primitiveID);
		if(!primitive)
			return;
		SE_ImageData* imageData = SE_Application::getInstance()->getResourceManager()->getImageData("TVscreen");
		//primitive->setImageData(imageData, SE_Texture::TEXTURE0, NOT_OWN);
		primitive->setImageData(imgd, SE_Texture::TEXTURE0, OWN, SE_ImageDataPortion(0, 0, imgd->getWidth() / 2, imgd->getHeight() / 2));
		SE_Mesh** meshArray = NULL;
		int meshNum = 0;
		primitive->createMesh(meshArray, meshNum);
		SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
        SE_Camera* camera = SE_Application::getInstance()->getCurrentCamera();
		SE_Quat q;
		q.set(90, SE_Vector3f(1, 0, 0));
		for(int i = 0 ; i < meshNum ; i++)
		{
		    SE_MeshSimObject* simObj = new SE_MeshSimObject(meshArray[i], OWN);
		    simObj->setName("rect primitive");
		    SE_SpatialID spatialID = SE_ID::createSpatialID();
            SE_Geometry* geometry = new SE_Geometry(spatialID, root);
		    root->addChild(geometry);
		    geometry->attachSimObject(simObj);
		    SE_Vector3f v = camera->getLocation();
		    v  = v + SE_Vector3f(0, 10, 0);
		    geometry->setLocalTranslate(v);
		    geometry->setLocalRotate(q);
		    geometry->setLocalScale(SE_Vector3f(4, 4, 4));
		    geometry->updateWorldTransform();
            LOGI("## left ##\n");
		}
		if(meshArray)
		{
			delete[] meshArray;
		}
		SE_BoxPrimitive* boxPrimitive = NULL;
		SE_PrimitiveID boxPrimitiveID;
		SE_BoxPrimitive::create(SE_Vector3f(1, 1, 1), boxPrimitive, boxPrimitiveID);
		//boxPrimitive->SE_ImageData* imageData = SE_Application::getInstance()->getResourceManager()->getImageData("TVscreen");
		//primitive->setImageData(imageData, SE_Texture::TEXTURE0, NOT_OWN);
		boxPrimitive->setImageData(SE_BoxPrimitive::ALL, imageData, SE_Texture::TEXTURE0, NOT_OWN);
		boxPrimitive->createMesh(meshArray, meshNum);
		SE_SpatialID groupSpatialID = SE_Application::getInstance()->createCommonID();
	    groupNode = new SE_CommonNode(groupSpatialID, root);
		root->addChild(groupNode);
		SE_Vector3f v = camera->getLocation();
		v = v + SE_Vector3f(0, 25, 0);
		//v = SE_Vector3f(0, -50, v.z);
		groupNode->setLocalTranslate(v);
		groupNode->setLocalRotate(q);
		for(int i = 0 ; i < meshNum ; i++)
		{
			SE_Mesh* mesh = meshArray[i];
            SE_SpatialID spatialID = SE_Application::getInstance()->createCommonID();
			SE_Geometry* geometry = new SE_Geometry(spatialID, groupNode);
			groupNode->addChild(geometry);
			SE_MeshSimObject* simObj = new SE_MeshSimObject(mesh, OWN);
		    simObj->setName("rect primitive");
			geometry->attachSimObject(simObj);
		}
		groupNode->updateWorldTransform();
		SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();
        startPos = v;//SE_Vector3f(0, 50, 0);
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameRIGHT))
    {
		mPhysics = new SE_Physics;
		mPhysics->setStartPos(startPos);
		mPhysics->initPhysics();
        LOGI("## right ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameUP))
    {
		/*
		if(mPhysics)
		    mPhysics->exitPhysics();
			*/
		SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		SE_SkinJointController* skinJointController = resourceManager->getSkinJointController("objLoft07");
		SE_BoneAnimation* anim = new SE_BoneAnimation();
		SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
		SE_SimObject* simobj = simObjectManager->findByName("objLoft07");
		SE_Spatial* spatial = simobj->getSpatial();
		anim->setRunMode(SE_Animation::NOT_REPEAT);
		anim->setTimeMode(SE_Animation::SIMULATE);
		anim->setDuration(1000);
		anim->setSimObject(simobj);
		anim->setSkinJointController(skinJointController);
		SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		animManager->removeAnimation(spatial->getAnimationID());
		SE_AnimationID animID = animManager->addAnimation(anim);
		spatial->setAnimationID(animID);
		anim->run();
  	    LOGI("## up ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameDOWN))
    {
	    LOGI("## down ##\n");
    }
}
bool SEDemo::RenderScene()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
    handleInput(dwCurrentWidth, dwCurrentHeight);
	/*
	if(mPhysics)
	{
	    mPhysics->stepSimulation(1.0f / 60);
	    SE_Matrix4f m = mPhysics->getObjMatrix();
		SE_Vector3f v = m.getTranslate();
		if(groupNode)
		{
			groupNode->setLocalTranslate(v);
			groupNode->updateWorldTransform();
		}
		for(int i = 0 ; i < 4 ; i++)
		{
			SE_Vector4f v = m.getColumn(i);
			LOGI("## %d : %f %f %f %f\n", i, v.x, v.y, v.z, v.w);
		}
	}
	*/
	SE_Application::getInstance()->run();
	int messageCount = SE_Application::getInstance()->getMessageCount();
	if(messageCount > 0)
	{
	    SE_Application::_MessageVector messageVector = SE_Application::getInstance()->getMessage();
		for(int i = 0 ; i < messageVector.size() ; i++)
		{
			SE_Message* msg = messageVector[i];
			LOGI("### msg type = %d ####\n", msg->type);
			SE_Struct* structData = msg->data;
			int structItemSize = structData->getCount();
			LOGI("### struct item size = %d ####\n", structItemSize);
			SE_StructItem* item = structData->getStructItem(0);
			SE_DataItem di = item->getDataItem(0);
			SE_StdString* strData = (SE_StdString*)di.data.virtualData;
			LOGI("#### obj name = %s #### \n", strData->data.c_str());
		}
		SE_Application::getInstance()->releaseMessage();
	}
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

