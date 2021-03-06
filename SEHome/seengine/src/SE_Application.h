#ifndef SE_APPLICATION_H
#define SE_APPLICATION_H
#include "SE_Time.h"
#include "SE_ID.h"
#include "SE_Command.h"
#include "SE_Message.h"
#include "SE_DelayDestroy.h"
#include "SE_Mutex.h"
#include <list>
#include <string>
#include <vector>
#include <map>
class SE_CommandFactory;
class SE_Camera;
class SE_ResourceManager;
class SE_SceneManager;
class SE_RenderManager;
class SE_InputManager;
class SE_AnimationManager;
class SE_ElementManager;
class SE_SimObjectManager;
class SE_RenderTargetManager;
class SE_CameraManager;
class SE_ParamManager;
class SE_SpatialManager;
class SE_ThreadManager;
class SE_FontManager;
class SE_DelayDestroy;
class SE_Game;
class SE_FunctionDict;
class SE_Application
{
public:
    enum {PREPARE, RUNNING, SUSPEND, EXIT};
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
	size_t getMessageCount();
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
	void setFrameRate(int frameRate)
	{
		mFrameRate = frameRate;
	}
	int getFrameRate()
	{
		return mFrameRate;
	}
    int getState() const
    {
        return mState;
    }
    void setState(int s)
    {
        mState = s;
    }
    SE_CommonID createCommonID();
    SE_FunctionDict* getFunctionDict()
    {
        return mFunctionDict;
    }
    SE_ResourceManager* getResourceManager()
	{
		return mResourceManager;
	}
    SE_SceneManager* getSceneManager()
	{
		return mSceneManager;
	}
	SE_RenderManager* getRenderManager()
	{
		return mRenderManager;
	}
	SE_InputManager* getInputManager()
	{
		return mInputManager;
	}
	SE_AnimationManager* getAnimationManager()
	{
		return mAnimationManager;
	}
	SE_ElementManager* getElementManager()
	{
		return mElementManager;
	}
    SE_SimObjectManager* getSimObjectManager()
    {
        return mSimObjectManager;
    }
	SE_RenderTargetManager* getRenderTargetManager()
	{
		return mRenderTargetManager;
	}
	SE_CameraManager* getCameraManager()
	{
		return mCameraManager;
	}
	SE_ParamManager* getParamManager()
	{
		return mParamManager;
	}
	SE_SpatialManager* getSpatialManager()
	{
		return mSpatialManager;
	}
	SE_FontManager* getFontManager()
	{
		return mFontManager;
	}
    SE_ThreadManager* getThreadManager()
    {
        return mThreadManager;
    }
    void addGame(std::string gameName, SE_Game* game);
    void removeGame(std::string gameName);
    SE_Game* getGame(std::string gameName);
    static SE_Application* getInstance();
	SE_Camera* getMainCamera();
	//index 0 to max camera num
	void setCamera(int index, SE_Camera* camera);
	SE_Camera* getCamera(int index);
	void setCurrentCamera(int index);
	SE_Camera* getCurrentCamera();
    //if return is false please delete dd by yourself
    bool addDelayDestroy(SE_DelayDestroy* dd);
	size_t getSeqNum()
	{
		return mSeqNum++;
	}
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
    void doDelayDestroy();
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
    class _FindDelayDestroy
    {
    public:
        bool operator()(SE_DelayDestroy* dd) const
        {
            if(*src == *dd)
                return true;
            else
                return false;
        }
        SE_DelayDestroy* src;
    };

    //typedef std::list<_CommanDWrapper> SE_CommandList;
    typedef std::list<SE_Command*> SE_CommandList;
    typedef std::list<_CommandFactoryEntry> SE_CommandFactoryList;
	typedef std::list<SE_Message*> SE_MessageList;
    typedef std::list<SE_DelayDestroy*> SE_DelayDestroyList;
    SE_Camera* mCameraArray[MAX_CAMERA_NUM];
    SE_Camera* mCurrentCamera;
    SE_SceneManager* mSceneManager;
    SE_ResourceManager* mResourceManager;
    SE_RenderManager* mRenderManager;
	SE_InputManager* mInputManager;
	SE_AnimationManager* mAnimationManager;
	SE_ElementManager* mElementManager;
    SE_SimObjectManager* mSimObjectManager;
	SE_RenderTargetManager* mRenderTargetManager;
	SE_CameraManager* mCameraManager;
	SE_ParamManager* mParamManager;
	SE_SpatialManager* mSpatialManager;
	SE_FontManager* mFontManager;
    SE_FunctionDict* mFunctionDict;
    SE_ThreadManager* mThreadManager;
    int mFrameNum;
    int mFrameRate;
    SE_TimeMS mStartTime;
    SE_TimeMS mPrevTime;
    SE_CommandList mCommandList;
	SE_Mutex mCommandListMutex;
    SE_CommandFactoryList mCommandFactoryList;
    SE_DelayDestroyList mDelayDestroyList;
    bool mStarted;
    int mFpsFrameNum;
    SE_TimeMS mFpsPrevTime;
    SE_APPID mAppID;
    int mState;
	int mObjectCount;
	SE_MessageList mMessageList;
	size_t mSeqNum;
    static SE_Application* mInstance;
    typedef std::map<std::string, SE_Game*> _GameMap;
    _GameMap mGameMap;
};
#define SE_GET_RESOURCEMANAGER() SE_Application::getInstance()->getResourceManager()
#define SE_GET_ELEMENTMANAGER() SE_Application::getInstance()->getElementManager()
#define SE_GET_SPATIALMANAGER() SE_Application::getInstance()->getSpatialManager()
#define SE_GET_SCENEMANAGER() SE_Application::getInstance()->getSceneManager()
#define SE_GET_RENDERTARGETMANAGER() SE_Application::getInstance()->getRenderTargetManager()
#define SE_GET_ANIMATIONMANAGER() SE_Application::getInstance()->getAnimationManager()
#define SE_GET_THREADMANAGER() SE_Application::getInstance()->getThreadManager();
#endif
