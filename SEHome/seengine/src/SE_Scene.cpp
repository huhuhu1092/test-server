#include "SE_Scene.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ElementManager.h"
#include "SE_RenderTargetManager.h"
#include "SE_RenderTarget.h"
#include "SE_CameraManager.h"
#include "SE_SpatialManager.h"
#include "SE_RenderState.h"
#include "SE_Spatial.h"
#include "SE_SpatialTravel.h"
#include "SE_Geometry3D.h"
#include "SE_Camera.h"
SE_Scene::SE_Scene(SE_SCENE_TYPE t)
{
    mWidth = mHeight = 0;
    mIsTranslucent = false;
    mSceneType = t;
	mIsModel = true;
}
SE_Scene::~SE_Scene()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    elementManager->remove(mRoot);
    SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
    cameraManager->removeCamera(mCamera);
}
void SE_Scene::create(const char* sceneName)
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_Element* element = resourceManager->loadScene(sceneName);
    SE_Element* root = NULL;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(mSceneType == SE_2D_SCENE)
    {
        root = new SE_2DNodeElement;
        SE_Vector4f c1(1, 0, 0, 0);
        SE_Vector4f c2(0, -1, 0, 0);
        SE_Vector4f c3(0, 0, 1, 0);
        SE_Vector4f c4(-mWidth / 2, mHeight / 2, 0, 1);
        SE_Matrix4f localM;
        localM.setColumn(0, c1);
        localM.setColumn(1, c2);
        localM.setColumn(2, c3);
        localM.setColumn(3, c4);
        root->setPostMatrix(localM);
		elementManager->add(root, element);
    }
    else
    {
        root = element;
    }
    mRoot = elementManager->add(SE_ElementID::NULLID, root, false);
    root->spawn();
    root->layout();
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
    SE_Element* rootElement = elementManager->get(mRoot);
    if(rootElement)
    {
		SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
        SE_Spatial* spatial = rootElement->createSpatial();
		spatialManager->add(SE_SpatialID::NULLID, spatial, false);
		spatial->updateSpatialIDToElement();
		SE_DepthTestState* rs = new SE_DepthTestState();
		rs->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
		SE_BlendState* blendRs = new SE_BlendState;
		blendRs->setBlendProperty(SE_BlendState::BLEND_ENABLE);
		blendRs->setBlendSrcFunc(SE_BlendState::SRC_ALPHA);
		blendRs->setBlendDstFunc(SE_BlendState::ONE_MINUS_SRC_ALPHA);
		spatial->setRenderState(SE_Spatial::DEPTHTESTSTATE, rs, OWN);
		spatial->setRenderState(SE_Spatial::BLENDSTATE, blendRs, OWN);
		spatial->updateWorldTransform();
		spatial->updateWorldLayer();
		spatial->updateRenderState();
    }
}
SE_Spatial* SE_Scene::getRootSpatial()
{
	SE_Element* rootElement = getRootElement();
	if(rootElement)
	{
	    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	    SE_Spatial* rootSpatial = NULL;
	    rootSpatial = spatialManager->get(rootElement->getSpatialID());
		return rootSpatial;
	}
	else
		return NULL;
}
SE_Element* SE_Scene::getRootElement()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* rootElement = elementManager->get(mRoot);
    return rootElement;
}
void SE_Scene::exit()
{}
void SE_Scene::hide()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* rootElement = elementManager->get(mRoot);
    if(rootElement)
        rootElement->hide();
}
void SE_Scene::render(const SE_SceneRenderSeq& seq, SE_RenderManager& renderManager)
{
    SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    if(mRenderTargetID == SE_RenderTargetID::INVALID)
    {
        SE_RenderTarget* renderTarget = new SE_FrameBufferTarget;
        
        mRenderTargetID = renderTargetManager->add(renderTarget);
    }        
    SE_Element* rootElement = getRootElement();
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	SE_Spatial* rootSpatial = NULL;
	if(rootElement)
	    rootSpatial = spatialManager->get(rootElement->getSpatialID());

    if(rootSpatial)
    {
        rootElement->setSceneRenderSeq(seq);
        rootElement->setRenderTargetID(mRenderTargetID);
	    SE_RenderTarget* renderTarget = renderTargetManager->get(mRenderTargetID);
        renderTarget->setBackground(mBackground);
        if(mIsTranslucent)
            renderTarget->setClearTarget(false);
        else
            renderTarget->setClearTarget(true);
        renderTarget->setCamera(mCamera);
		SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
		SE_Camera* camera = cameraManager->getCamera(mCamera);
		rootSpatial->renderScene(camera, &renderManager);
    }
}
void SE_Scene::setCamera(const SE_CameraID& cameraID)
{
    mCamera = cameraID;
}
bool SE_Scene::dispatchKeyEvent(const SE_KeyEvent& keyEvent)
{
    return false;
}
SE_Element* SE_Scene::getPointedElement(float x, float y)
{
    SE_Spatial* rootSpatial = getRootSpatial();
	if(!rootSpatial)
		return NULL;
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_Camera* camera = cameraManager->getCamera(mCamera);
	if(camera == NULL)
		return NULL;
	SE_Ray ray = camera->screenCoordinateToRay(x, y);
    SE_FindSpatialCollision spatialCollision(ray);
	rootSpatial->travel(&spatialCollision, true);
	SE_Spatial* collisionSpatial = spatialCollision.getCollisionSpatial();
	SE_ElementID elementID = collisionSpatial->getElementID();
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Element* e = elementManager->get(elementID);
	return e;
}
bool SE_Scene::dispatchMotionEvent(const SE_MotionEvent& motionEvent)
{
	return false;
}
/*
void SE_Scene::setID(const SE_SceneID& sceneID)
{
    mID = sceneID;
}
*/
