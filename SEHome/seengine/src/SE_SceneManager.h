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
    SE_SceneManager();
    ~SE_SceneManager();
    // render a scene to render manager which will render it to render target
    void renderScene(SE_Camera* camera, SE_RenderManager& renderManager);
    SE_Spatial* getRoot();
    SE_Spatial* find(const SE_SpatialID& spatialID);
    void createScene(const char* sceneFileName);
    void updateSpatialIDMap();
	void setSelectedSpatial(SE_Spatial* spatial);
	SE_Spatial* getSelectedSpatial()
	{
		return mSelectedSpatial;
	}
private:
	SE_SceneManager(const SE_SceneManager&);
	SE_SceneManager& operator=(const SE_SceneManager&);
private:
    SE_Spatial* mSceneRoot;
	SE_Spatial* mSelectedSpatial;
    struct SpatialIDMap;
    SpatialIDMap* mSpatialIDMap;
};
#endif
