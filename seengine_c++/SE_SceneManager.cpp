#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"
struct SE_SceneManager::SpatialIDMap
{
    typedef std::map<SE_SpatialID, SE_Spatial*> SMap;
    SMap map;
};
SE_SceneManager::SE_SceneManager()
{
	mSceneRoot = NULL;
    mSpatialIDMap = new SE_SceneManager::SpatialIDMap;
}
SE_SceneManager::~SE_SceneManager()
{
    delete mSceneRoot;
    delete mSpatialIDMap;
}
void SE_SceneManager::renderScene(SE_Camera* camera, SE_RenderManager& renderManager)
{
    SE_Matrix4f perspectiveMatrix = camera->getPerspectiveMatrix();
    renderManager.setPerspectiveMatrix(perspectiveMatrix);
    renderManager.setWorldToViewMatrix(camera->getWorldToViewMatrix());
    mSceneRoot->renderScene(camera, &renderManager);
}
SE_CommonNode* SE_SceneManager::getRoot()
{
    return mSceneRoot;
}
SE_Spatial* SE_SceneManager::find(const SE_SpatialID& spatialID)
{
	return NULL;
}
void SE_SceneManager::createScene(const char* sceneFileName)
{
	if(mSceneRoot != NULL)
		delete mSceneRoot;
    mSceneRoot = new SE_CommonNode(SE_Application::getInstance()->createCommonID(), NULL);
	SE_Spatial* scene = SE_Application::getInstance()->getResourceManager()->loadScene(sceneFileName);
	scene->setParent(mSceneRoot);
	mSceneRoot->addChild(scene);
}
void SE_SceneManager::updateSpatialIDMap()
{}

