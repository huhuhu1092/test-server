#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
class SE_Scene;
class SE_SceneManager
{
public:
    SE_SceneID addScene(SE_Scene* scene);
    SE_SceneID topScene();
    void popScene();
    void rotateScene();
    void swapScene();
private:
    SE_TreeStructManager<SE_Scene> mScenes;
    std::list<SE_SceneID> mStack;
};
#endif
