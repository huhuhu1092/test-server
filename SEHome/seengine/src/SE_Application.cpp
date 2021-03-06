#include "SE_Application.h"
#include "SE_SceneManager.h"
#include "SE_ResourceManager.h"
#include "SE_RenderManager.h"
#include "SE_Camera.h"
#include "SE_Command.h"
#include "SE_CommandFactory.h"
#include "SE_RenderManager.h"
#include "SE_InputManager.h"
#include "SE_AnimationManager.h"
#include "SE_ElementManager.h"
#include "SE_SimObjectManager.h"
#include "SE_RenderTargetManager.h"
#include "SE_CameraManager.h"
#include "SE_ParamManager.h"
#include "SE_SpatialManager.h"
#include "SE_FontManager.h"
#include "SE_DelayDestroy.h"
#include "SE_ThreadManager.h"
#include "SE_Game.h"
#include "SE_FunctionDict.h"
#include "SE_Log.h"
#include <string.h>
#include <algorithm>
SE_Application* SE_Application::mInstance = NULL;
SE_Application* SE_Application::getInstance()
{
    if(mInstance == NULL)
    {
        mInstance = new SE_Application;
    }
    return mInstance;
}
SE_Application::SE_Application()
{
    memset(mCameraArray, 0, sizeof(SE_Camera*) * MAX_CAMERA_NUM);
    mCurrentCamera = NULL;
    mSceneManager = new SE_SceneManager;
    mResourceManager = new SE_ResourceManager;
    mRenderManager = new SE_RenderManager;
	mInputManager = new SE_InputManager;
	mAnimationManager = new SE_AnimationManager;
	mElementManager = new SE_ElementManager;
    mSimObjectManager = new SE_SimObjectManager;
	mRenderTargetManager = new SE_RenderTargetManager;
	mCameraManager = new SE_CameraManager;
	mParamManager = new SE_ParamManager;
	mSpatialManager = new SE_SpatialManager;
	mFontManager =  new SE_FontManager;
    mFunctionDict = new SE_FunctionDict;
    mThreadManager = new SE_ThreadManager;
    mFrameNum = 0;
    mStartTime = 0;
    mPrevTime = 0;
    mFrameRate = 30;
    mStarted = false;
    mFpsPrevTime = 0;
    mFpsFrameNum = 0;
	mObjectCount = 0;
    mState = PREPARE;
	mSeqNum = 0;
}
SE_Application::~SE_Application()
{
    for(int i = 0 ; i < MAX_CAMERA_NUM ; i++)
    {
        if(mCameraArray[i])
        {
            delete mCameraArray[i];
        }        
    }
    if(mSceneManager)
        delete mSceneManager;
    if(mResourceManager)
        delete mResourceManager;
	if(mInputManager)
		delete mInputManager;
	if(mAnimationManager)
		delete mAnimationManager;
	if(mElementManager)
		delete mElementManager;
    if(mSimObjectManager)
        delete mSimObjectManager;
	if(mRenderTargetManager)
		delete mRenderTargetManager;
	if(mCameraManager)
		delete mCameraManager;
	if(mParamManager)
		delete mParamManager;
	if(mSpatialManager)
		delete mSpatialManager;
	if(mFontManager)
		delete mFontManager;
    if(mFunctionDict)
        delete mFunctionDict;
    if(mThreadManager)
        delete mThreadManager;
    SE_CommandFactoryList::iterator it;
    for(it = mCommandFactoryList.begin() ; it != mCommandFactoryList.end() ; it++)
    {
        delete it->factory;
    }
    delete mRenderManager;
}
void SE_Application::doDelayDestroy()
{
    SE_DelayDestroyList::iterator it;
    for(it = mDelayDestroyList.begin() ; it != mDelayDestroyList.end() ; it++)
    {
        SE_DelayDestroy* dd = *it;
        dd->destroy();
        delete dd;
    }
    mDelayDestroyList.clear();
}
void SE_Application::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    processCommand(realDelta, simulateDelta);
	mAnimationManager->update(realDelta, simulateDelta);
    if(mState == RUNNING)
    {
        //mRenderManager->beginDraw();
        mSceneManager->render(*mRenderManager);
        mRenderManager->sort();
        mRenderManager->draw();
        mRenderManager->endDraw();
    }
    doDelayDestroy();
}
void SE_Application::start()
{}
void SE_Application::shutdown()
{
    if(mResourceManager)
    {
        mResourceManager->releaseHardwareResource();
    }

}
void SE_Application::run()
{
    if(!mStarted)
    {
        mStarted = true;
        mStartTime = SE_Time::getCurrentTimeMS();
        mPrevTime = mStartTime;
    }
    SE_TimeMS currTime = SE_Time::getCurrentTimeMS();
    SE_TimeMS delta = currTime - mPrevTime;
    mPrevTime = currTime;
    update(delta, mFrameRate);
    mFrameNum++;   
    SE_TimeMS fpsDelta = currTime - mFpsPrevTime;
    mFpsFrameNum++;
    if(fpsDelta > 1000)
    {
        float fFPS = 1000.0f * mFpsFrameNum /(float)fpsDelta;
        //LOGI("FPS : %f\n", fFPS);
        mFpsFrameNum = 0;
        mFpsPrevTime = currTime;
    }
}
void SE_Application::setCamera(int index, SE_Camera* camera)
{
	if(index < 0 || index >= MAX_CAMERA_NUM)
		return;
    SE_Camera* c = mCameraArray[index];
	if(c)
		delete c;
	mCameraArray[index] = camera;
}
SE_Camera* SE_Application::getCamera(int index)
{
	if(index < 0 || index >= MAX_CAMERA_NUM)
		return NULL;
	return mCameraArray[index];
}
void SE_Application::setCurrentCamera(int index)
{
	if(index < 0 || index >= MAX_CAMERA_NUM)
		return;
	mCurrentCamera = mCameraArray[index];
}
SE_Camera* SE_Application::getCurrentCamera()
{
	return mCurrentCamera;
}
void SE_Application::sendCommand(SE_Command* command)
{
    if(command)
    {
        command->handle(0, 0);
        delete command;
    }
}
SE_CommonID SE_Application::createCommonID()
{
    SE_TimeUS currTime = SE_Time::getCurrentTimeUS();
	return SE_CommonID(mAppID.first, mAppID.second, (unsigned int)currTime, mObjectCount++);
}
/*
class isPriorityLessThan
{
public:
    isPriorityLessThan(int priority) : mPriority(priority)
    {
    }
	bool operator()(const SE_Application::_CommandWrapper& cw)
    {
        if(cw.command->priority() < mPriority)
            return true;
        else 
            return false;
    }
private:
    int mPriority;
};
*/
void SE_Application::postCommand(SE_Command* command)
{
	SE_AutoMutex m(&mCommandListMutex);
    mCommandList.push_back(command);
}
void SE_Application::setUpEnv()
{}
bool SE_Application::isRemoved(const _CommandWrapper& c)
{
    if(c.canDelete)
        return true;
    else
        return false;
}
void SE_Application::processCommand(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_CommandList::iterator it;
	mCommandListMutex.lock();
    SE_CommandList tmpList = mCommandList;
    mCommandList.clear();
	mCommandListMutex.unlock();
    for(it = tmpList.begin(); it != tmpList.end(); )
    {
        SE_Command* c = *it;
        if(c->expire(realDelta, simulateDelta))
        {
            c->handle(realDelta, simulateDelta);
            delete c;
            tmpList.erase(it++);
        }
		else
		{
			it++;
		}
    }
    if(tmpList.empty())
        return;
	mCommandListMutex.lock();
    mCommandList.splice(mCommandList.begin(), tmpList, tmpList.begin(), tmpList.end()); 
	mCommandListMutex.unlock();
}

bool SE_Application::registerCommandFactory(const SE_CommandFactoryID& cfID, SE_CommandFactory* commandFactory)
{
    _CommandFactoryEntry fe;
    fe.id = cfID;
    fe.factory = commandFactory;
    SE_CommandFactoryList::iterator it = find_if(mCommandFactoryList.begin(), mCommandFactoryList.end(), isCommandFactoryIDEqual(cfID));
    if(it != mCommandFactoryList.end())
        return false;
    mCommandFactoryList.push_back(fe);
    return true;
}
bool SE_Application::unreginsterCommandFactory(const SE_CommandFactoryID& cfID)
{
    remove_if(mCommandFactoryList.begin(), mCommandFactoryList.end(), isCommandFactoryIDEqual(cfID));
    return true;
}
SE_Command* SE_Application::createCommand(const SE_CommandID& commandID)
{
    SE_CommandFactoryList::iterator it;
    for(it = mCommandFactoryList.begin(); it != mCommandFactoryList.end(); it++)
    {
        _CommandFactoryEntry e = *it;
        SE_Command* command = e.factory->create(this, commandID);
        if(command)
            return command;
    }
	return NULL;
}
bool SE_Application::addDelayDestroy(SE_DelayDestroy* dd)
{
    _FindDelayDestroy fd;
    fd.src = dd;
    SE_DelayDestroyList::iterator it = find_if(mDelayDestroyList.begin(), mDelayDestroyList.end(), fd);
    if(it != mDelayDestroyList.end())
    {
        return false;
    }
    mDelayDestroyList.push_back(dd);
    return true;
}
void SE_Application::sendMessage(SE_Message* message)
{
	mMessageList.push_back(message);
}
size_t SE_Application::getMessageCount()
{
	return mMessageList.size();
}
void SE_Application::releaseMessage()
{
	SE_MessageList::iterator it;
	for(it = mMessageList.begin() ; it != mMessageList.end() ; it++)
	{
		SE_Message* msg = *it;
		delete msg;
	}
	mMessageList.clear();
}
SE_Application::_MessageVector SE_Application::getMessage()
{
    _MessageVector v;
	v.resize(mMessageList.size());
	SE_MessageList::iterator it;
	int i = 0;
	for(it = mMessageList.begin() ; it != mMessageList.end() ; it++)
	{
		SE_Message* msg = *it;
		v[i++] = msg;
	}
	return v;
}
void SE_Application::addGame(std::string gameName, SE_Game* game)
{
    _GameMap::iterator it = mGameMap.find(gameName);
    if(it == mGameMap.end())
    {
        mGameMap.insert(std::pair<std::string, SE_Game*>(gameName, game));
    }
    else
    {
        SE_Game* g = it->second;
        SE_DelayDestroy* dd = new SE_DelayDestroyPointer<SE_Game>(g);
        bool ret = addDelayDestroy(dd);
        if(!ret)
        {
            delete dd;
        }
        it->second = game;
    }
}
void SE_Application::removeGame(std::string gameName)
{
    _GameMap::iterator it = mGameMap.find(gameName);
    if(it != mGameMap.end())
    {
        SE_Game* game = it->second;
        mGameMap.erase(it);
        SE_DelayDestroy* dd = new SE_DelayDestroyPointer<SE_Game>(game);
        bool ret = addDelayDestroy(dd);
        if(!ret)
        {
            delete dd;
        }
    }
}
SE_Game* SE_Application::getGame(std::string gameName)
{
     _GameMap::iterator it = mGameMap.find(gameName);
    if(it != mGameMap.end())
    {
        return it->second;
    }   
    else
        return NULL;
}
