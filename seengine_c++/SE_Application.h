#ifndef SE_APPLICATION_H
#define SE_APPLICATION_H
#include "SE_Time.h"
#include "SE_ID.h"
#include <list>
class SE_Application
{
public:
    enum {MAX_CAMERA_NUM = 16};
    typedef int SE_APPID;
    virtual ~SE_Application();
    void run();
    void sendCommand(SE_Command* command);
    void postCommand(SE_Command* command);
    bool registerCommandFactory(const SE_CommandFactoryID& cfID, SE_CommandFactory* commandFactory);
    bool unreginsterCommandFactory(const SE_CommandFactoryID& cfID);
    SE_Command* createCommand(const SE_CommandID& commandID);
    void setAppID(SE_APPID id)
    {
        mAppID = id;
    }
    SE_APPID getAppID()
    {
        return mAppID;
    }
    SE_CommonID createCommonID();
    SE_ResourceManager* getResourceManager();
    SE_SceneManager* getSceneManager();
    static SE_Application* getInstance();
protected:
    virtual void setUpEnv();
    virtual void processCommand(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    SE_Application();
protected:
    class _CommandWrapper
    {
    public:
        _CommandWrapper(SE_Command* c) : command(c)
        {
            canDelete = false;
            canDestroy = true;
        }
        ~_CommandWrapper()
        {
            if(canDestroy)
                delete command;
        } 
        SE_Command* command;
        bool canDelete;
        bool canDestroy;
    };
    struct _CommandFactoryEntry
    {
        SE_CommandFactoryID id;
        SE_CommandFactory* factory;
    };
    //typedef std::list<_CommanDWrapper> SE_CommandList;
    typedef std::list<SE_Command*> SE_CommandList;
    typedef std::list<_CommandFactoryEntry> SE_CommandFactoryList;
    SE_Camera* mCameraArray[MAX_CAMERA_NUM];
    SE_Camera* mCurrentCamera;
    SE_SceneManager* mSceneManager;
    SE_ResourceManager* mResourceManager;
    int mFrameNum;
    int mFrameRate;
    SE_TimeMS mStartTime;
    SE_TimeMS mPrevTime;
    SE_CommandList mCommandList;
    SE_CommandFactoryList mCommandFactoryList;
    bool mStarted;
    SE_APPID mAppID;
    static SE_Application* mInstance;
};
#endif
