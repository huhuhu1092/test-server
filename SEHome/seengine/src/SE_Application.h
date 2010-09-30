#ifndef SE_APPLICATION_H
#define SE_APPLICATION_H
#include "SE_Time.h"
#include "SE_ID.h"
#include "SE_Command.h"
#include "SE_Message.h"
#include <list>
#include <string>
#include <vector>
class SE_CommandFactory;
class SE_Camera;
class SE_ResourceManager;
class SE_SceneManager;
class SE_RenderManager;
class SE_InputManager;
class SE_AnimationManager;
class SE_Application
{
public:
    enum {MAIN_CAMERA = 0, MAX_CAMERA_NUM = 16};
    struct SE_APPID
	{
		int first;
		int second;
	};
	typedef std::vector<SE_Message*> _MessageVector;
    virtual ~SE_Application();
    void run();
    void start();
    void shutdown();
    void sendCommand(SE_Command* command);
    void postCommand(SE_Command* command);
	void sendMessage(SE_Message* message);
	int getMessageCount();
	//after getMessage , the message's pointer are copy to _MessageVector
	_MessageVector getMessage();
	void releaseMessage();
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
    SE_ResourceManager* getResourceManager()
	{
		return mResourceManager;
	}
    SE_SceneManager* getSceneManager()
	{
		return mSceneManager;
	}
	SE_InputManager* getInputManager()
	{
		return mInputManager;
	}
    static SE_Application* getInstance();
	SE_Camera* getMainCamera();
	//index 0 to max camera num
	void setCamera(int index, SE_Camera* camera);
	SE_Camera* getCamera(int index);
	void setCurrentCamera(int index);
	SE_Camera* getCurrentCamera();
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

    virtual void setUpEnv();
    virtual void processCommand(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    SE_Application();
	bool isRemoved(const _CommandWrapper& c);
protected:
    struct _CommandFactoryEntry
    {
        SE_CommandFactoryID id;
        SE_CommandFactory* factory;
    };
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
    //typedef std::list<_CommanDWrapper> SE_CommandList;
    typedef std::list<SE_Command*> SE_CommandList;
    typedef std::list<_CommandFactoryEntry> SE_CommandFactoryList;
	typedef std::list<SE_Message*> SE_MessageList;
    SE_Camera* mCameraArray[MAX_CAMERA_NUM];
    SE_Camera* mCurrentCamera;
    SE_SceneManager* mSceneManager;
    SE_ResourceManager* mResourceManager;
    SE_RenderManager* mRenderManager;
	SE_InputManager* mInputManager;
	SE_AnimationManager* mAnimationManager;
    int mFrameNum;
    int mFrameRate;
    SE_TimeMS mStartTime;
    SE_TimeMS mPrevTime;
    SE_CommandList mCommandList;
    SE_CommandFactoryList mCommandFactoryList;
    bool mStarted;
    int mFpsFrameNum;
    SE_TimeMS mFpsPrevTime;
    SE_APPID mAppID;
	int mObjectCount;
	SE_MessageList mMessageList;
    static SE_Application* mInstance;
};
#endif
