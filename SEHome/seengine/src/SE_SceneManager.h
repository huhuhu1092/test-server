#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
#include "SE_ID.h"
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
#endif
