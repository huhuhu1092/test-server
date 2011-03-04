#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_Scene.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"
#include "SE_InputEvent.h"
#include "SE_ElementManager.h"
#include "SE_Math.h"
#include "SE_Log.h"
SE_SceneManager::SE_SceneManager() : mScenes(SE_SceneManager::SIZE, SE_SceneManager::MAX_SIZE)
{
    mWidth = mHeight = 0;
	mPrevX = 0 ;
	mPrevY = 0;
	mPrevMotionEventType = SE_MotionEvent::UP;
	mCursorScene = NULL;
}
SE_SceneManager::~SE_SceneManager()
{

}
void SE_SceneManager::loadCursor(const char* cursorResource)
{
	if(mCursorScene)
	{ 
        delete mCursorScene;  
	}
	mCursorScene = new SE_Scene(SE_2D_SCENE);
	mCursorScene->setBound(mWidth, mHeight);
	SE_Camera* camera = SE_Camera::create2DSceneCamera(mWidth, mHeight);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();

}
SE_SceneID SE_SceneManager::add(SE_Scene* scene)
{
    SE_SceneID id = mScenes.add(SE_SceneID::NULLID, scene);
    return id;
}
SE_SceneID SE_SceneManager::top()
{
    _SceneStack::iterator it = mStack.begin();
    if(it != mStack.end())
    {
        return *it;
    }
    else
        return SE_SceneID::NULLID;
}
SE_Scene* SE_SceneManager::get(const SE_SceneID& id)
{
    return mScenes.find(id);
}
void SE_SceneManager::pop()
{
    mStack.pop_front();
}
void SE_SceneManager::rotate()
{}
void SE_SceneManager::swap()
{}
void SE_SceneManager::show(const SE_SceneID& id)
{
    SE_Scene* scene = get(id);
    if(scene == NULL)
        return;
    if(id == top())
        return;
    _SceneStack::iterator it;
    for(it = mStack.begin() ; it != mStack.end() ; it++)
    {
        if(id == *it)
            break;
    }
    if(it != mStack.end())
        mStack.erase(it);
    mStack.push_front(id);
    scene->show();
}
void SE_SceneManager::hide(const SE_SceneID& id)
{

}
void SE_SceneManager::dismiss(const SE_SceneID& id)
{}
void SE_SceneManager::render(SE_RenderManager& renderManager)
{
    std::list<SE_Scene*> sceneNeedRender;
    _SceneStack::iterator it;
    for(it = mStack.begin() ; it != mStack.end() ; it++)
    {
        SE_SceneID sid = *it;
        SE_Scene* scene = get(sid);
        if(scene)
        {
            if(scene->isTranslucent())
            {
                sceneNeedRender.push_front(scene);
            }
            else
            {
                sceneNeedRender.push_front(scene);
                break;
            }
        }
    }    
    if(sceneNeedRender.size() >= SE_MAX_RENDERSCENE_SIZE)
    {
        LOGE("scene size exceed the max size\n");
        return;
    }
    int seq = sceneNeedRender.size() - 1;
    std::list<SE_Scene*>::iterator itScene;
    SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
    for(itScene = sceneNeedRender.begin() ; itScene != sceneNeedRender.end() ; itScene++)
    {
        SE_Scene* scene = *itScene;
        //SE_Camera* camera = cameraManager->findCamera(scene->getCamera());
        //if(camera)
        //{
        //    renderManager->invalidScene(seq);
        //    renderManager->setSceneCamera(seq, camera);
            scene->render(seq, renderManager);
            seq--;
        //}
    }
}
void SE_SceneManager::dispatchKeyEvent(const SE_KeyEvent& keyEvent)
{

}
void SE_SceneManager::handleMotionEvent(SE_Element* pointedElement, const SE_MotionEvent& motionEvent)
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	if(mPrevMotionEventType == SE_MotionEvent::UP && motionEvent.getType() == SE_MotionEvent::DOWN)
	{
		mPrevMotionEventType = SE_MotionEvent::DOWN;
		mPrevX = motionEvent.getX();
		mPrevY = motionEvent.getY();
		if(pointedElement)
		{
	        mMotionDownElementID = pointedElement->getID();
	        pointedElement->setState(SE_Element::HIGHLIGHTED, true);
		}
		LOGI("#### motion event down ###\n");
	}
	else if((mPrevMotionEventType == SE_MotionEvent::DOWN || mPrevMotionEventType == SE_MotionEvent::MOVE)&& 
		    motionEvent.getType() == SE_MotionEvent::DOWN)
	{
        float deltaX = motionEvent.getX() - mPrevX;
		float deltaY = motionEvent.getY() - mPrevY;
		if(mPrevMotionEventType == SE_MotionEvent::DOWN && 
			(SE_Fabs(deltaX) > SE_MotionEvent::MOVE_SLOPE || SE_Fabs(deltaY) > SE_MotionEvent::MOVE_SLOPE))
		{
			mPrevMotionEventType = SE_MotionEvent::MOVE;
		}
		if(mPrevMotionEventType == SE_MotionEvent::MOVE)
		{
			LOGI("#### motion event move ###\n");
			if(pointedElement)
			    mMotionMoveElementID = pointedElement->getID();
			if(mMotionMoveElementID != mMotionDownElementID)
			{
				SE_Element* e = elementManager->get(mMotionDownElementID);
				if(e)
				{
					e->setState(SE_Element::NORMAL, true);
				}
			}
			mPrevX = motionEvent.getX();
			mPrevY = motionEvent.getY();
		}
 	}
	else if(motionEvent.getType() == SE_MotionEvent::UP && mPrevMotionEventType == SE_MotionEvent::MOVE)
	{
		if(pointedElement)
		    mMotionMoveElementID = pointedElement->getID();
		mPrevMotionEventType = SE_MotionEvent::UP;
	    if(mMotionMoveElementID != mMotionDownElementID)
		{
			SE_Element* e = elementManager->get(mMotionDownElementID);
			if(e)
			{
				e->setState(SE_Element::NORMAL, true);
			}
		}
	}
	else if(motionEvent.getType() == SE_MotionEvent::UP && mPrevMotionEventType == SE_MotionEvent::DOWN)
	{
		LOGI("#### motion event up ###\n");
		if(pointedElement)
            mMotionUpElementID = pointedElement->getID();
		mPrevMotionEventType = SE_MotionEvent::UP;
		if(mMotionDownElementID == mMotionUpElementID && pointedElement)
		{
			LOGI("#### motion event click ###\n");
			pointedElement->setState(SE_Element::NORMAL, true);
			pointedElement->click();
		}
		else
		{
			SE_Element* e = elementManager->get(mMotionDownElementID);
			if(e)
			{
				e->setState(SE_Element::NORMAL, true);
			}
		}
	}   
}
void SE_SceneManager::dispatchMotionEvent(const SE_MotionEvent& motionEvent)
{
    std::list<SE_Scene*> sceneMotionEvent;
    _SceneStack::iterator it;
    for(it = mStack.begin() ; it != mStack.end() ; it++)
    {
        SE_SceneID sid = *it;
        SE_Scene* scene = get(sid);
        if(scene)
        {
			if(!scene->isModel())
            {
                sceneMotionEvent.push_back(scene);
            }
            else
            {
                sceneMotionEvent.push_back(scene);
                break;
            }
        }
    } 
	std::list<SE_Scene*>::iterator itScene;
	SE_SceneRenderSeq sceneRenderSeq = -1;
	SE_Element* pointedElement = NULL;
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	for(itScene = sceneMotionEvent.begin() ; itScene != sceneMotionEvent.end(); itScene++)
	{
		SE_Scene* scene = *itScene;
		SE_Element* e = scene->getPointedElement(motionEvent.getX(), motionEvent.getY());
		if(e)
		{
            if(e->getSceneRenderSeq() > sceneRenderSeq)
			{
				pointedElement = e;
				sceneRenderSeq = e->getSceneRenderSeq();
			}
		}
	}
    handleMotionEvent(pointedElement, motionEvent);
}

