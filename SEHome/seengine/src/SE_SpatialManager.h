#ifndef SE_SPATIALMANAGER_H
#define SE_SPATIALMANAGER_H
#include "SE_ID.h"
#include "SE_TreeStructManager.h"
class SE_Spatial;
class SE_RenderManager;
/*
class SE_Spatial;
class SE_Camera;
class SE_CommonNode;
class SE_RenderManager;
class SE_SceneManager
{
public:
	enum SE_ERROR {SE_NO_ERROR, CHILD_INVALID_ID, CHILD_DUP_ID};
    SE_SceneManager();
    ~SE_SceneManager();
    // render a scene to render manager which will render it to render target
    void renderScene(SE_Camera* camera, SE_RenderManager& renderManager);
    SE_Spatial* getRoot();
    void setRoot(SE_Spatial* spatial);
    SE_Spatial* find(const SE_SpatialID& spatialID);
    //add a spatial child to scene manager, it must indicate a parent
	//the parent must be added in scene
	//if parent is NULL , child is the child of scene root.
	//if parent is NULL and scene root is NULL, child will be scene root
	void addSpatial(SE_Spatial* parent, SE_Spatial* child);
	SE_Spatial* removeSpatial(const SE_SpatialID& spatialID);
    void checkSpatialIDMap();
	SE_ERROR getError();
    void createScene(const SE_SceneID& sceneFileName);
    void updateSpatialIDMap();
	void setSelectedSpatial(SE_Spatial* spatial);
	SE_Spatial* getSelectedSpatial()
	{
		return mSelectedSpatial;
	}
public:
    struct SpatialIDMap;
private:
	SE_SceneManager(const SE_SceneManager&);
	SE_SceneManager& operator=(const SE_SceneManager&);
	void setError(SE_ERROR e);
private:
    SE_Spatial* mSceneRoot;
	SE_Spatial* mSelectedSpatial;
    SpatialIDMap* mSpatialIDMap;
};
*/
class SE_SpatialCreator
{
public:
	virtual ~SE_SpatialCreator() {}
	virtual SE_Spatial* create(int type);
};
class SE_SpatialManager
{
public:
	enum {SE_NO_ERROR, SE_SPATIAL_NOT_EXIST};
    SE_SpatialManager();
	~SE_SpatialManager();
	int getError() const
	{
		return mError;
	}
	SE_Spatial* findSpatial(const SE_SpatialID& id);
	SE_Spatial* removeSpatial(const SE_SpatialID& id);
	SE_SpatialID addSpatial(const SE_SpatialID& parentID, SE_Spatial* spatial, bool linkToParent);
	void addSpatial(SE_Spatial* parent, SE_Spatial* child);
    //SE_SpatialID loadScene(const SE_SceneID& sceneID); 
	void render(SE_Camera* camera, SE_RenderManager& renderManager);
	//SE_SpatialID getCurrentSceneRootID() const;
	//void setCurrentSceneRootID(const SE_SpatialID& id);
	//SE_Spatial* getCurrentSceneRoot() const;
	SE_Spatial* createSpatial(int spatialType);
	void setSpatialCreator(SE_SpatialCreator* c)
	{
		if(mSpatialCreator)
			delete mSpatialCreator;
		mSpatialCreator = c;
	}
	SE_SpatialCreator* getSpatialCreator() const
	{
		return mSpatialCreator;
	}
	void release(const SE_SpatialID& id, int delay = SE_RELEASE_DELAY);
	void release(SE_Spatial* spatial, int delay = SE_RELEASE_DELAY);
	std::vector<SE_Spatial*> getChildren(const SE_SpatialID& id) ;
	SE_Spatial* getParent(const SE_SpatialID& id) ;
private:
	SE_SpatialManager(const SE_SpatialManager&);
	SE_SpatialManager& operator=(const SE_SpatialManager&);	
private:
	//SE_SpatialID mCurrentSceneRoot;
	int mError;
	SE_TreeStructManager<SE_Spatial> mSpatials;
	SE_SpatialCreator* mSpatialCreator;
};
#endif
