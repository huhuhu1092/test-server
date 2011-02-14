#include "SE_Scene.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ElementManager.h"
#include "SE_RenderTargetManager.h"
#include "SE_RenderTarget.h"
#include "SE_CameraManager.h"
#include "SE_SpatialManager.h"
SE_Scene::SE_Scene()
{
    mX = mY = mWidth = mHeight = 0;
    mIsTranslucent = false;
}
SE_Scene::~SE_Scene()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    elementManager->removeElement(mRoot);
    SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
    cameraManager->removeCamera(mCamera);
}
void SE_Scene::create(const char* sceneName)
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_Element* rootElement = resourceManager->loadScene(sceneName);
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    mRoot = elementManager->addElement(SE_ElementID::NULLID, rootElement);
    rootElement->spawn();
    rootElement->layout();
}
/*
SE_SceneID SE_Scene::getID()
{
    return mID;
}
*/
void SE_Scene::show()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* rootElement = elementManager->findElement(mRoot);
    if(rootElement)
    {
		SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
        SE_Spatial* spatial = rootElement->createSpatial();
		spatialManager->addSpatial(SE_SpatialID::NULLID, spatial);

    }
}
SE_Element* SE_Scene::getRootElement()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* rootElement = elementManager->findElement(mRoot);
    return rootElement;
}
void SE_Scene::exit()
{}
void SE_Scene::hide()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* rootElement = elementManager->findElement(mRoot);
    if(rootElement)
        rootElement->hide();
}
void SE_Scene::render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager)
{
    SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    if(mRenderTargetID == SE_RenderTargetID::INVALID)
    {
        SE_RenderTarget* renderTarget = new SE_FrameBufferTarget;
        
        mRenderTargetID = renderTargetManager->addRenderTarget(renderTarget);
    }        
    SE_Element* rootElement = getRootElement();
    if(rootElement)
    {
        rootElement->setSceneRenderSeq(seq);
        rootElement->setRenderTargetID(mRenderTargetID);
    }
    SE_RenderTarget* renderTarget = renderTargetManager->getRenderTarget(mRenderTargetID);
    renderTarget->setBackground(mBackground);
    if(mIsTranslucent)
        renderTarget->setClearTarget(false);
    else
        renderTarget->setClearTarget(true);
    renderTarget->setCamera(mCamera);
}
void SE_Scene::setCamera(const SE_CameraID& cameraID)
{
    mCamera = cameraID;
}
void SE_Scene::dispatchKeyEvent(const SE_KeyEvent& keyEvent)
{}
void SE_Scene::dispatchMotionEvent(const SE_MotionEvent& motionEvent)
{}
/*
void SE_Scene::setID(const SE_SceneID& sceneID)
{
    mID = sceneID;
}
*/
