#ifndef SE_SCENEMANAGER_H
#define SE_SCENEMANAGER_H
#include "SE_TreeStructManager.h"
#include <list>
class SE_Scene;
class SE_KeyEvent;
class SE_MotionEvent;
class SE_RenderManager;
/*
 * SceneManager manage the all screen region
 * its geometry is (0, 0, mWidth, mHeight)
 * Scene in SceneManager will be placed in this region
*/
class SE_SceneManager
{
public:
    enum {MAX_SIZE = 2000};
    enum {SIZE = 200};
    SE_SceneManager();
    ~SE_SceneManager();
    SE_SceneID addScene(SE_Scene* scene);
    SE_SceneID topScene();
    SE_Scene* findScene(const SE_SceneID& id);
    void popScene();
    void rotateScene();
    void swapScene();
    void showScene(const SE_SceneID& id);
    void hideScene(const SE_SceneID& id);
    void dismissScene(const SE_SceneID& id);
    void renderScene(SE_RenderManager& renderManager);
    void dispatchKeyEvent(const SE_KeyEvent& keyEvent);
    void dispatchMotionEvent(const SE_MotionEvent& motionEvent);
    void setSceneWidth(float width)
    {
        mWidth = width;
    }
    void setSceneHeight(float height)
    {
        mHeight = height;
    }

private:
    SE_SceneManager(const SE_SceneManager&);
    SE_SceneManager& operator=(const SE_SceneManager&);
private:
    SE_TreeStructManager<SE_Scene> mScenes;
    typedef std::list<SE_SceneID> _SceneStack;
    _SceneStack mStack;
    float mWidth;
    float mHeight;
};
#endif
