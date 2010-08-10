#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
struct SE_SceneManager::SpatialIDMap
{
    typedef std::map<SE_SpatialID, SE_Spatial*> SMap;
    SMap map;
};
SE_SceneManager::SE_SceneManager()
{
    mSceneRoot = new SE_CommonNode(SE_Application::getInstance()->createCommonID(), NULL);
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
{}
void SE_SceneManager::updateSpatialIDMap()
{}

