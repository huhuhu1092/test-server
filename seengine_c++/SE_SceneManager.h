#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
class SE_Spatial;
class SE_Node;
class SE_Camera;
class SE_RenderManager;
classs SE_SceneManager
{
public:
    SE_SceneManager();
    ~SE_SceneManager();
    // render a scene to render manager which will render it to render target
    void renderScene(SE_Camera* camera, SE_RenderManager& renderManager);
    SE_CommonNode* getRoot();
    SE_Spatial* find(const SE_SpatialID& spatialID);
    void createScene(const char* sceneFileName);
    void updateSpatialIDMap();
private:
    SE_CommonNode* mSceneRoot;
    struct SpatialIDMap;
    SpatialIDMap* mSpatialIDMap;
};
#endif
