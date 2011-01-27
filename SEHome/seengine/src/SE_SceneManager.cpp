#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"
SE_SceneManager::SE_SceneManager() : mScenes(SE_SceneManager::SIZE, SE_SceneManager::MAX_SIZE)
{
    mWidth = mHeight = 0;
}
SE_SceneManager::~SE_SceneManager()
{

}
SE_SceneID SE_SceneManager::addScene(SE_Scene* scene)
{
    SE_SceneID id = mScenes.add(SE_SceneID::NULLID, scene);
    return id;
}
SE_SceneID SE_SceneManager::topScene()
{
    _SceneStack::iterator it = mStack.begin();
    if(it != mStack.end())
    {
        return *it;
    }
    else
        return SE_SceneID::NULLID;
}
SE_Scene* SE_SceneManager::findScene(const SE_SceneID& id)
{
    return mScenes.find(id);
}
void SE_SceneManager::popScene()
{
    mStack.pop_front();
}
void SE_SceneManager::rotateScene()
{}
void SE_SceneManager::swapScene()
{}
void SE_SceneManager::showScene(const SE_SceneID& id)
{
    SE_Scene* scene = findScene(id);
    if(scene == NULL)
        return;
    if(id == topScene())
        return;
    _SceneStack::iterator it;
    for(it = mStack.begin() ; it != mStack.end() ; it++)
    {
        if(id == *it)
            break;
    }
    if(it != mStack.end())
        mStack.erase(it);
    mStack.push_front();
    scene->show();
}
void SE_SceneManager::hideScene(const SE_SceneID& id)
{

}
void SE_SceneManager::dismissScene(const SE_SceneID& id)
{}
void SE_SceneManager::renderScene(SE_RenderManager& renderManager)
{}
void SE_SceneManager::dispatchKeyEvent(const SE_KeyEvent& keyEvent)
{}
void SE_SceneManager::dispatchMotionEvent(const SE_MotionEvent& motionEvent)
{}

