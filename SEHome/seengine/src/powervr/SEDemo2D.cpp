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
#include "SE_MessageEventCommandDefine.h"
//#include "SE_Physics.h"
#include "SE_2DCommand.h"
#include "SE_ElementManager.h"
#include "SE_AnimationManager.h"
#include "SE_Animation.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_CChess.h"
#include "SE_Remote.h"
#include "SE_ChessCommand.h"
#include "SE_Primitive.h"
#include "SE_Scene.h"
#include "SE_Thread.h"
#include "SE_ThreadManager.h"
#include "SE_MessageStream.h"
#include "SE_NetDataCommand.h"
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
////////////////////////////////////////////

///////////////////////////////////
static void drawScene(int width, int height)
{
}

class SEDemo : public PVRShell
{
public:
	SEDemo()
	{
		//mPhysics = NULL;
		mSelectedSpatial = NULL;
	}
	virtual bool InitApplication();
	virtual bool InitView();
	virtual bool ReleaseView();
	virtual bool QuitApplication();
	virtual bool RenderScene();
private:
	void handleInput(int width, int height);
private:
	//SE_Physics* mPhysics;
    SE_Spatial* mSelectedSpatial;
	SE_CChess* mChessApp;
	SE_ThreadID mChessAIThread;
};
static void doTest()
{
	struct testTraverse
	{
		void operator()(SE_Spatial* s)
		{
			SE_SpatialID id = s->getID();
		}
	};
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	SE_CommonNode* cn = new SE_CommonNode;
	for(int i = 0 ; i < 10 ; i++)
	{
        SE_Spatial* s = new SE_Spatial;
		spatialManager->add(cn, s);
	}
	SE_SpatialID ii = spatialManager->add(SE_SpatialID::NULLID, cn, false);
	SE_Spatial* s = spatialManager->remove(ii);
	spatialManager->release(s);
	//spatialManager->check();
}
static void debugPrimitive()
{
    SE_Primitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    float e[2] = {1, 1};

    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
  
    SE_RectPrimitive::create(rect3D, 3.2, 3.2, primitive, primitiveID);

}
bool SEDemo::InitApplication()
{
	//doTest();
	//PVRShellSet(prefWidth, SCREEN_WIDTH);
	//PVRShellSet(prefHeight, SCREEN_HEIGHT);
    //debug
    debugPrimitive();
    //end
	SE_Application::SE_APPID appid;
	appid.first = 137;
	appid.second = 18215879;
	SE_Application::getInstance()->setAppID(appid);
    
	SE_CChess* chessapp = new SE_CChess(30, 690, 53, 53, SE_CChess::RED, SE_CChess::BLACK);
    chessapp->setUserName("aa");
    chessapp->setPassword("aa");
	SE_Remote remoteInfo;
	remoteInfo.setServerIP("192.168.5.102");
	remoteInfo.setServerPort(80);
	remoteInfo.setNetwork(true, false, false);
	chessapp->setRemote(remoteInfo);
    SE_Application::getInstance()->addGame("cchess", chessapp);
	SE_SystemCommandFactory* sf = new SE_SystemCommandFactory;
	SE_Application::getInstance()->registerCommandFactory("SystemCommand", sf);
	SE_Init2D* c = new SE_Init2D(SE_Application::getInstance());
	//SE_InitAppCommand* c = (SE_InitAppCommand*)SE_Application::getInstance()->createCommand("SE_InitAppCommand");
#ifdef WIN32
	c->dataPath = "D:\\model\\newhome3";//"D:\\model\\jme\\home\\newhome3";
#else
	c->dataPath = "/home/luwei/model/newhome3";
#endif
	c->sceneName = "ChessLayout.xml/ChessRoot";//"TestElement.xml/PFemaleBase";
	c->chessApp = chessapp;
	c->left = 0;
	c->top = 0;
#ifdef ROTATE
	c->width = 800;
	c->height = 480;
#else
	c->width = 480;
	c->height = 800;
#endif
	SE_Application::getInstance()->postCommand(c);
	return true;
}
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	LOGI("## width = %d, height = %d ###\n", dwCurrentWidth,dwCurrentHeight);
	SE_2DUpdateCameraCommand* c = new SE_2DUpdateCameraCommand(SE_Application::getInstance());
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
#if defined(WIN32)
#else
    SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
    SE_ChessLogoutThread* thread = new SE_ChessLogoutThread(false);
    thread->username = chessApp->getUserName();
    thread->remoteInfo = chessApp->getRemote();
    thread->start();
	int i = 0 ;
    while(!thread->isEnd() && i < 100)
    {
        LOGI("### wait for server close response ###\n");
		i++;
    }
    delete thread;
    LOGI("##### quit OK ###\n");
#endif
	return true;
}
/*
class SE_RunAllAnimationTravel : public SE_SpatialTravel
{
public:
    int visit(SE_Spatial* spatial)
    {
		SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
		SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		SE_ElementID elementID = spatial->getElementID();
		SE_AnimationID animID = spatial->getAnimationID();
		animManager->removeAnimation(animID);
		SE_Element* element = elementManager->findByID(elementID);
		if(element)
		{
			SE_Animation* anim = element->getAnimation();
			SE_Animation* cloneAnim = NULL;
			if(anim)
			{
				cloneAnim = anim->clone();
				animID = animManager->addAnimation(cloneAnim);
				spatial->setAnimationID(animID);
				cloneAnim->setRunMode(SE_Animation::REPEAT);
				cloneAnim->run();
			}
		}
		return 0;
    }
    int visit(SE_SimObject* simObject)
	{
        return 1;
	}
};
class SE_PauseAllAnimationTravel : public SE_SpatialTravel
{
public:
	int visit(SE_Spatial* spatial)
	{
        SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		SE_AnimationID animID = spatial->getAnimationID();
		SE_Animation* anim = animManager->getAnimation(animID);
		if(anim)
		{
			anim->setRunMode(SE_Animation::ONE_FRAME);
			anim->pause();
		}
		return 0;
	}
	int visit(SE_SimObject* simObject)
	{
        return 0;
	}
};
*/
void SEDemo::handleInput(int width, int height)
{
    static float prevPointer[2];
    static bool bPressed = false;
	bool equal = false;
    int buttonState = PVRShellGet(prefButtonState);
    float* pointerLocation = (float*)PVRShellGet(prefPointerLocation);
    /*LOGI("## buttonstate = %d ##\n", buttonState);*/
    if(pointerLocation)
    {
		//LOGI("### pointer location = %f, %f ###\n", pointerLocation[0], pointerLocation[1]);
		if(prevPointer[0] == pointerLocation[0] && prevPointer[1] == pointerLocation[1] )
			equal = true;
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
	
	    SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
	    if(c)
	    {
#ifdef ROTATE
            SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::DOWN , prevPointer[1] * height, ( 1 - prevPointer[0] ) * width);
#else
		    SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::DOWN, prevPointer[0] * width, prevPointer[1] * height);
#endif
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
#ifdef ROTATE
            SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::UP , prevPointer[1] * height, ( 1 - prevPointer[0] ) * width);

#else
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::UP, prevPointer[0] * width, prevPointer[1] * height);
#endif	
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
        bPressed = 0;
    }
    if(PVRShellIsKeyPressed(PVRShellKeyNameLEFT))
    {
		/*
		if(mSelectedSpatial)
		{
            SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
			SE_Element* element = elementManager->findByName(mSelectedSpatial->getElementID().getStr());
            if(element)
            {
                SE_Animation* anim = element->getAnimation();
				if(anim)
				{
					SE_Animation* newAnim = anim->clone();
                    SE_AnimationManager* animationManager = SE_Application::getInstance()->getAnimationManager();
					SE_AnimationID animID = mSelectedSpatial->getAnimationID();
					animationManager->removeAnimation(animID);
					animID = animationManager->addAnimation(newAnim);
			        mSelectedSpatial->setAnimationID(animID);
                    newAnim->run();
				}
			}
        }
		else
		{
			SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	        SE_Spatial* root = sceneManager->getRoot();
	        SE_RunAllAnimationTravel rat;
	        root->travel(&rat, true);
		}
		*/
		//SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
		//SE_Element* root = elementManager->getRoot();
		//root->startAnimation();
		SE_SceneManager* sceneManager = SE_GET_SCENEMANAGER();
		SE_SceneID sceneID = sceneManager->top();
		SE_Scene* scene = sceneManager->get(sceneID);
		SE_Element* root = scene->getRootElement();
		root->startAnimation();
        LOGI("## left ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameRIGHT))
    {
		/*
		if(mSelectedSpatial)
		{
		    SE_AnimationID animID = mSelectedSpatial->getAnimationID();
		    SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		    SE_Animation* anim = animManager->getAnimation(animID);
			if(anim)
		        anim->nextFrame(30, 30);
		}
		*/
        LOGI("## right ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameUP))
    {

		/*
		if(mSelectedSpatial)
		{
			SE_PauseAllAnimationTravel rat;
			mSelectedSpatial->travel(&rat, true);
		}
		else
		{
            SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	        SE_Spatial* root = sceneManager->getRoot();
	        SE_PauseAllAnimationTravel rat;
	        root->travel(&rat, true);
		}
		*/
  	    LOGI("## up ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameDOWN))
    {
		/*
		SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
		sceneManager->setSelectedSpatial(NULL);
		mSelectedSpatial = NULL;
		*/
		SE_NetMessage* netMessage = new SE_NetMessage;
		std::string str = "D:\\model\\tmp\\aa\\image\\\Female_dress_001_M.png";
		netMessage->len = 3 + str.size();
		netMessage->data = new unsigned char[netMessage->len];
		netMessage->data[0] = 0;
		short s = SE_Util::host2NetInt16(netMessage->len);
		memcpy(netMessage->data + 1, &s, 2);
		memcpy(netMessage->data + 3, str.c_str(), str.size());
		SE_NetDataCommand* netCommand = new SE_NetDataCommand(SE_Application::getInstance(), netMessage);
		SE_Application::getInstance()->postCommand(netCommand);
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
			if(msg->type == SE_MSG_SIMOBJECT_NAME)
			{
				//mSelectedSpatial = SE_Application::getInstance()->getSceneManager()->getSelectedSpatial();
			}
			SE_Struct* structData = msg->data;
			int structItemSize = structData->getCount();
			LOGI("### struct item size = %d ####\n", structItemSize);
			SE_StructItem* item = structData->getStructItem(0);
			SE_Value di = item->getDataItem(0);
			SE_StdString* strData = (SE_StdString*)di.getVirtualData();
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

