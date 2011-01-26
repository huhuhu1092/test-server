#ifndef SE_SCENE_H
#define SE_SCENE_H
class SE_SceneManager;
class SE_RenderManager;
class SE_Scene
{
    friend class SE_SceneManager;
public:
    void create(const char* sceneName);
    SE_SceneID getID();
    SE_ElementID getRoot();
    void show();
    void exit();
    void hide();
    void render(SE_RenderManager& renderManager);
    void setCamera(const SE_CameraID& cameraID);
    void dispatchKey();
    void dispatchPointer();
private:
    void setID(const SE_SceneID& sceneID);
private:
    SE_SceneID mID;
    SE_ElementID mRoot;
    SE_CameraID mCamera;
};
#endif
