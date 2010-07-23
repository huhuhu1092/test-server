#include "SE_SceneManager.h"
#include "SE_Node.h"
struct SE_SceneManager::SpatialIDMap
{
    typedef std::map<SE_SpatialID, SE_Spatial*> SMap;
    SMap map;
};
SE_SceneManager::SE_SceneManager()
{
    mSceneRoot = SE_Node()
}
SE_SceneManager::~SE_SceneManager()
{}
void SE_SceneManager::renderScene(SE_Camera* camera, SE_RenderManager& renderManager)
{}
SE_Spatial* SE_SceneManager::getRoot()
{}
SE_Spatial* SE_SceneManager::find(const SE_SpatialID& spatialID)
{}
void SE_SceneManager::createScene(const char* sceneFileName)
{}
void SE_SceneManager::updateSpatialIDMap()
{}

