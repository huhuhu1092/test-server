#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
#include "SE_ID.h"
#include <list>
#include <vector>
class SE_Spatial;
class SE_Camera;
class SE_CommonNode;
class SE_RenderManager;
class SE_RenderTarget;
class SE_MotionEventController;
class SE_KeyEventController;
class SE_MotionEvent;
class SE_KeyEvent;
class SE_Scene
{
public:
	enum SE_ERROR {SE_NO_ERROR, CHILD_INVALID_ID, CHILD_DUP_ID};
	enum SE_SCENE_VISIBILITY {SE_VISIBLE, SE_NOVISIBLE};
    SE_Scene(const SE_StringID& sceneName, SE_SCENE_TYPE t = SE_FRAMEBUFFER_SCENE);
    ~SE_Scene();
    SE_StringID getName() const
    {
        return mName;
    }
    SE_SCENE_TYPE getType() const
    {
        return mType;
    }
	void setVisibility(SE_SCENE_VISIBILITY s)
	{
		mVisibility = s;
	}
	SE_SCENE_VISIBILITY getVisibility() const
	{
		return mVisibility;
	}
    void setCamera(SE_Camera* camera);
    SE_Camera* getCamera() const
    {
        return mCamera;
    }
    void setRenderTarget(SE_RenderTarget* renderTarget);
    SE_RenderTarget* getRenderTarget() const
    {
        return mRenderTarget;
    }
    void setNeedDraw(bool bDraw)
    {
        mNeedDraw = bDraw;
    }
    bool needDraw() const
    {
        return mNeedDraw;
    }
    bool setIsTranslucent(bool translucent)
    {
        mIsTranslucent = translucent;
    }
    bool isTranslucent() const
    {
        return mIsTranslucent;
    }
    void setBackgroundColor(const SE_Vector4f& c)
    {
        mBackgroundColor = c;
    }
    SE_Vector4f getBackgroundColor() const
    {
        return mBackgroundColor;
    }
	void setCanHandleInput(bool b)
	{
		mCanHandleInput = b;
	}
	bool canHandleInput() const
	{
		return mCanHandleInput;
	}
    void setMotionEventController(SE_MotionEventController* obj);
    void releaseMotionEventController();
    SE_MotionEventController* getMotionEventController() const;
    void setKeyEventController(SE_KeyEventController* obj);
    void releaseKeyEventController();
    SE_KeyEventController* getKeyEventController() const;
    // render a scene to render manager which will render it to render target
    void renderScene(SE_RenderManager& renderManager);
    SE_Spatial* getRoot();
    void setRoot(SE_Spatial* spatial);
    void createRoot(const char* sceneFileName);
    SE_Spatial* find(const SE_SpatialID& spatialID);
    //add a spatial child to scene manager, it must indicate a parent
	//the parent must be added in scene
	//if parent is NULL , child is the child of scene root.
	//if parent is NULL and scene root is NULL, child will be scene root
	void addSpatial(SE_Spatial* parent, SE_Spatial* child);
	SE_Spatial* removeSpatial(const SE_SpatialID& spatialID);
    void checkSpatialIDMap();
	SE_ERROR getError();
    void updateSpatialIDMap();

    void setSkeletonState();

	void setSelectedSpatial(SE_Spatial* spatial);
	SE_Spatial* getSelectedSpatial()
	{
		return mSelectedSpatial;
	}
    void unLoadScene();
    void dispatchMotionEvent(SE_MotionEvent* motionEvent);
    void dispatchKeyEvent(SE_KeyEvent* keyEvent);
public:
    struct SpatialIDMap;
private:
	SE_Scene(const SE_Scene&);
	SE_Scene& operator=(const SE_Scene&);
	void setError(SE_ERROR e);
private:
    SE_Spatial* mSceneRoot;
	SE_Spatial* mSelectedSpatial;
    SpatialIDMap* mSpatialIDMap;
    SE_StringID mName;
    SE_SCENE_TYPE mType;
    SE_Camera* mCamera;
    SE_RenderTarget* mRenderTarget;
    bool mNeedDraw;
    bool mIsTranslucent;
    SE_Vector4f mBackgroundColor;
	bool mCanHandleInput;
	SE_SCENE_VISIBILITY mVisibility;
	SE_MotionEventController* mMotionEventController;
	SE_KeyEventController* mKeyEventController;
};
/*
 * scene manager contain all scene in it
 * it has resposibility to release scene
 * other user can not release scene
 * it must invoke removeScene to remove and release scene
 * */
class SE_SceneManager
{
public:
    SE_SceneManager();
    ~SE_SceneManager();
    typedef std::vector<SE_Scene*> _SceneSet;
    void pushFront(SE_SCENE_TYPE t, SE_Scene* scene);
    void pushBack(SE_SCENE_TYPE t, SE_Scene* scene);
    void removeScene(SE_Scene* scene, bool isRelease = true);
    void removeScene(const SE_StringID& sceneName, bool isRelease = true);
    _SceneSet getScene(const SE_StringID& sceneName);
    SE_Scene* getScene(SE_SCENE_TYPE t, const SE_StringID& sceneName);
    void insert(SE_SCENE_TYPE t, int index, SE_Scene* scene);
	SE_Scene* getTopScene();
	void setMainScene(const SE_StringID& name);
	SE_Scene* getMainScene() const
	{
		return mMainScene;
	}
    void renderScene(SE_RenderManager& renderManager);
    _SceneSet getSceneFromRemovedList(const SE_StringID& sceneName, bool remove = true);
    SE_Scene* getSceneFromRemovedList(SE_SCENE_TYPE t, const SE_StringID& sceneName, bool remove = true);
    SE_Spatial* find(const SE_SpatialID& spatialID);
	SE_Spatial* removeSpatial(const SE_SpatialID& spatialID);
	void releaseVBO();
private:
    typedef std::list<SE_Scene*> _SceneList;
private:
    void clear();
    void renderScene(int& index, _SceneList* sceneList, SE_RenderManager& renderManager);
    void render(int& index , SE_Scene* scene, SE_RenderManager& renderManager);
    void renderFrameBufferScene(int& index, _SceneList* sceneList, SE_RenderManager& renderManager);
    bool isPostEffectSceneTranslucent();
	SE_SceneManager(const SE_SceneManager&);
	SE_SceneManager& operator=(const SE_SceneManager&);
private:
    SE_Scene* mMainScene;
    _SceneList mSceneListArray[SE_SCENE_TYPE_NUM];
    _SceneList mRemovedSceneListArray[SE_SCENE_TYPE_NUM];
};

#endif
