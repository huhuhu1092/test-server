#include "SE_SceneManager.h"
#include "SE_CommonNode.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"

/////////////////////////////////////
struct SE_SceneManager::SpatialIDMap
{
    typedef std::map<SE_SpatialID, SE_Spatial*> Map;
    Map spatialIDMap;
	SE_SceneManager* sceneManger;
	void add(const SE_SpatialID& spatialID, SE_Spatial* spatial)
	{
		Map::iterator it = spatialIDMap.find(spatialID);
		if(it == spatialIDMap.end())
			spatialIDMap[spatialID] = spatial;
		else
		{
			it->second = spatial;
		}
	}
	void remove(const SE_SpatialID& spatialID)
	{
		Map::iterator it = spatialIDMap.find(spatialID);
		if(it != spatialIDMap.end())
		    spatialIDMap.erase(it);
	}
	void clear()
	{
		spatialIDMap.clear();
	}
};
////////////////////////////
///////////////////////////////////
class _SpatialIDTravel : public SE_SpatialTravel
{
public:
	enum {ADD_SPATIAL, REMOVE_SPATIAL};
    int visit(SE_Spatial* spatial)
	{
		if(spatial->getSpatialID().isValid())
		{
			if(op == ADD_SPATIAL)
		        spatialIDMap->add(spatial->getSpatialID(), spatial);
			else if(op == REMOVE_SPATIAL)
				spatialIDMap->remove(spatial->getSpatialID());
		}
		return 0;
	}
    int visit(SE_SimObject* simObject)
	{
		return 0;
	}
	SE_SceneManager::SpatialIDMap* spatialIDMap;
	int op;
};
SE_SceneManager::SE_SceneManager()
{
	mSceneRoot = NULL;
	mSelectedSpatial = NULL;
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
    renderManager.sort();
}
SE_Spatial* SE_SceneManager::getRoot()
{
    return mSceneRoot;
}
void SE_SceneManager::setRoot(SE_Spatial* root)
{
    if(mSceneRoot)
        delete mSceneRoot;
    mSceneRoot = root;
}
void SE_SceneManager::setSelectedSpatial(SE_Spatial* spatial)
{
	if(mSelectedSpatial)
	{
		mSelectedSpatial->setSelected(false);
	}
	mSelectedSpatial = spatial;
	if(mSelectedSpatial)
	    mSelectedSpatial->setSelected(true);
}
SE_Spatial* SE_SceneManager::find(const SE_SpatialID& spatialID)
{
	SE_ASSERT(mSpatialIDMap);
	SE_SceneManager::SpatialIDMap::Map::iterator it = mSpatialIDMap->spatialIDMap.find(spatialID);
	if(it != mSpatialIDMap->spatialIDMap.end())
		return it->second;
	else
		return NULL;
}
void SE_SceneManager::createScene(const SE_SceneID& sceneID)
{
	if(mSceneRoot != NULL)
	{
		delete mSceneRoot;
	}
	mSceneRoot = SE_Application::getInstance()->getResourceManager()->getScene(sceneID);
	updateSpatialIDMap();
}
void SE_SceneManager::updateSpatialIDMap()
{
    mSpatialIDMap->clear();
    _SpatialIDTravel addSpatialIDTravel;
	addSpatialIDTravel.spatialIDMap = mSpatialIDMap;
	addSpatialIDTravel.op = _SpatialIDTravel::ADD_SPATIAL;
	if(mSceneRoot)
	    mSceneRoot->travel(&addSpatialIDTravel, true);    
}

void SE_SceneManager::addSpatial(SE_Spatial* parent, SE_Spatial* child)
{
	if(!child)
		return;
	if(parent == NULL)
	{
		if(mSceneRoot)
		{
		    mSceneRoot->addChild(child);
		    child->setParent(mSceneRoot);
		}
		else
		{
			mSceneRoot = child;
			child->setParent(NULL);
		}
	}
	else
	{
		parent->addChild(child);
		child->setParent(parent);
	}
	_SpatialIDTravel addSpatialIDTravel;
	addSpatialIDTravel.spatialIDMap = mSpatialIDMap;
	addSpatialIDTravel.op = _SpatialIDTravel::ADD_SPATIAL;
	child->travel(&addSpatialIDTravel, true);
}
SE_Spatial* SE_SceneManager::removeSpatial(const SE_SpatialID& spatialID)
{
	SE_Spatial* spatial = find(spatialID);
	if(!spatial)
		return NULL;
	SE_Spatial* retSpatial = spatial;
	SE_Spatial* parent = spatial->getParent();
	if(parent)
	{
		parent->removeChild(spatial);
	}
	else
	{
        mSceneRoot = NULL;
	}
	_SpatialIDTravel removeSpatialIDTravel;
	removeSpatialIDTravel.spatialIDMap = mSpatialIDMap;
	removeSpatialIDTravel.op = _SpatialIDTravel::REMOVE_SPATIAL;
	retSpatial->travel(&removeSpatialIDTravel, true);
	return retSpatial;
}
void SE_SceneManager::checkSpatialIDMap()
{}
