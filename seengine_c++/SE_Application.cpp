#include "SE_Application.h"
#include <string.h>
SE_Application::SE_Application()
{
    memset(mCameraArray, 0, sizeof(SE_Camera*) * MAX_CAMERA_NUM);
    mCurrentCamera = NULL;
    mSceneManager = new SE_SceneManager;
    mResourceManager = new SE_ResourceManager;
    mFrameNum = 0;
    mStartTime = 0;
    mCurrTime = 0;
    mFrameRate = 30;
    mStarted = false;
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
    SE_CommandFactoryList::iterator it;
    for(it = mCommandFactoryList.begin() ; it != mCommandFactoryList.end() ; it++)
    {
        delete it->factory;
    }
}
void SE_Application::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    processCommand(realDelta, simulateDelta);
    mSceneManager->renderScene(mCurrentCamera);
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
    update(mPrevTime, mFrameRate);
    mFrameNum++;   
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
    SE_TimeMS currTime = SE_Time::getCurrentTimeMS();
    return SE_CommonID(mAppID, (unsigned int)currTime, 0, 0);
}

class isPriorityLessThan
{
public:
    isPriorityLessThan(int priority) : mPriority(priority)
    {
    }
    bool operator()(const _CommandWrapper& cw)
    {
        if(cw.command->priority() < mPriority)
            return true;
        else 
            return false;
    }
private:
    int mPriority;
};
void SE_Application::postCommand(SE_Command* command)
{
    mCommandList.push_back(command);
    /*
    if(mCommandList.empty())
    {
        mCommandList.push_back(cw);
    }
    else
    {
        SE_ComandList::iterator it = find_if(mCommandList.begin(), mCommandList.end(), isPriorityLessThan(command->priority()));
        if(it != mCommandList.end())
        {
            mCommandList.insert(it, cw);
        }
        else
        {
            mCommandList.push_back(cw);
        }
    }
    */
}
void SE_Application::setUpEnv()
{}
bool isRemoved(const _CommandWrapper& c)
{
    if(c.canDelete)
        return true;
    else
        return false;
}
void SE_Application::processCommand(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_CommandList::iterator it;
    SE_CommandList tmpList = mCommandList;
    mCommandList.clear();
    for(it = tmpList.being(); it != tmpList.end(); it++)
    {
        SE_Command* c = *it;
        if(c->expire(realDelta, simulateDelta))
        {
            c->handle(realDelta, simulateDelta);
            delete c;
            it = tmpList.remove(it);
            if(it == tmpList.end())
                break;
            else
                --it;
        }
    }
    if(tmpList.empty())
        return;
    mCommandList.splice(mCommandList.begin(), tmpList, tmpList.begin(), tmpList.end()); 
    /*
    for(it = tmpList.being(); it != tmpList.end(); it++)
    {
        _CommandWrapper c = *it;
        if(c.command->expire(realDelta, simulateDelta))
        {
            c.command->handle(realDelta, simulateDelta);
            c.canDelete = true;
        }
    }
    tmpList.remove_if(isRemoved);
    if(tmpList.empty())
        return;
    if(mCommandList.empty())
    {
        mCommandList.assign(tmpList.begin(), tmpList().end());
    }
    else
    {
        SE_CommandList::iterator itSrc;
        SE_CommandList::iterator itDst;
        for(itSrc = tmpList.begin() ; itSrc != tmpList.end(); itSrc++)
        {
            for(itDst = mCommandList.begin() ; itDst != mCommandList.end() ; itDst++)
            {
                if(itSrc->command->priority() >= itDst->command->priority())
                {
                    break;
                }
            }
            mCommandList.insert(itDst, *itSrc);
            itSrc->canDestroy = false;
        } 
        tmpList.clear();
    }
    */
}
class isCommandFactoryIDEqual
{
public:
    isCommandFactoryIDEqual(const SE_CommandFactoryID& id): mID(id)
    {}
    bool operator()(const _CommandFactoryEntry& e)
    {
        if(mID == e.id)
            return true;
        else 
            return false;
    }
private:
    SE_CommandFactoryID mID;
};
bool SE_Application::registerCommandFactory(const SE_CommandFactoryID& cfID, SE_CommandFactory* commandFactory)
{
    _CommandFactoryEntry fe;
    fe.id = cfID;
    fe.factory = commandFactory;
    SE_CommandFactoryList::iterator it = find_if(mCommandfactoryList.begin(), mCommandFactoryList.end(), isCommandFactoryIDEqual(cfID));
    if(it != mCommandFactoryList.end())
        return false;
    mCommondFactoryList.push_back(fe);
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
    for(it = mCommandFactoryList.being(); it != mCommandFactoryList.end(); it++)
    {
        _CommandFactoryEntry e = *it;
        SE_Command* command = e.factory->create(this, commandID);
        if(command)
            return command;
    }
}
