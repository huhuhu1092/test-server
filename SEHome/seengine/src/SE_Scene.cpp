#include "SE_Scene.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
SE_Scene::SE_Scene()
{
    mX = mY = mWidth = mHeight = 0;
    mIsTranslucent = false;
}
SE_Scene::~SE_Scene()
{}
void SE_Scene::create(const char* sceneName)
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    mRoot = resourceManager->loadScene(sceneName);
}
SE_SceneID SE_Scene::getID()
{
    return mID;
}
void SE_Scene::show()
{}
void SE_Scene::exit()
{}
void SE_Scene::hide()
{}
void SE_Scene::render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager)
{

}
void SE_Scene::setCamera(const SE_CameraID& cameraID)
{
    mCamera = cameraID;
}
void SE_Scene::dispatchKeyEvent(const SE_KeyEvent& keyEvent)
{}
void SE_Scene::dispatchMotionEvent(const SE_MotionEvent& motionEvent)
{}
void SE_Scene::setID(const SE_SceneID& sceneID)
{
    mID = sceneID;
}
