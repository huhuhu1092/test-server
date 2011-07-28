#include "SE_CommonNode.h"
#include "SE_SceneManager.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"
#include "SE_SkinJointController.h"
#include "SE_BipedController.h"
#include "SE_SimObject.h"
#include "SE_Log.h"
#include "SE_Mesh.h"
#include "SE_RenderTarget.h"
#include "SE_Utils.h"
#include "SE_MotionEventController.h"
#include "SE_KeyEventController.h"
#include "SE_InputEvent.h"
#include <algorithm>
/////////////////////////////////////
struct SE_Scene::SpatialIDMap
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
	SE_Scene::SpatialIDMap* spatialIDMap;
	int op;
};

class _setBoneState : public SE_SpatialTravel
{
public:	
    int visit(SE_Spatial* spatial)
	{		
		return 0;
	}
    int visit(SE_SimObject* simObject)
	{
        if(!simObject)
        {
            return 0;
        }
        //generate skeleton info
        SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_SkeletonController *sk = resourceManager->getSkeletonController(SE_SKELETONCONTROLLER);

        if(sk)
        {
        int bipcontrollerNum = sk->mSkeletonController.size();

        for(int c = 0; c < bipcontrollerNum; ++c)
        {
            SE_BipedController * bipedController = sk->mSkeletonController[c];

            if(bipedController)
            {
                SE_SkeletonUnit *su = bipedController->findSU(simObject->getName());

                if(su)
                {
                    simObject->setBoneState(true);
                    simObject->setBoneController(bipedController);
                    simObject->setSkeletonUnit(su);

                    int surfaceNum = simObject->getSurfaceNum();

                    for(int i = 0; i < surfaceNum; ++i)
                    {
                        SE_Surface * surface = simObject->getMesh()->getSurface(i);

                        surface->setCurrentBipedControllerID(su->controllerId.c_str());

                        surface->setCurrentBipedController(bipedController);

                        _Vector3f* vertexT = NULL;
                        int vertexNumT = 0;
                        int* vertexIndexT = NULL;
                        int vertexIndexNumT = 0;

                        surface->getFaceVertex(vertexT, vertexNumT);

                        surface->getVertexIndexInGeometryData(vertexIndexT, vertexIndexNumT);

                        float *weightarray = new float[vertexIndexNumT * 4];//four bone info per vertex
                        float *indexarray = new float[vertexIndexNumT * 4];//four bone info per vertex


                        for(int j = 0; j < vertexIndexNumT; ++j)
                        {                      

                            for(int k = 0; k < 4; ++k)
                            {
                                if(su->objVertexBlendInfo[vertexIndexT[j]]->weight.size() > 4)
                                {
                                    LOGI("Error. skeleton num > 4 per vertex!\n");
                                }

                                if(k < su->objVertexBlendInfo[vertexIndexT[j]]->weight.size())
                                {
                                    int bipIndex = su->objVertexBlendInfo[vertexIndexT[j]]->bipedIndex[k];//bipIndex is start from 1, not 0.

                                    int bipindexfromcache = su->bipCache[bipIndex-1]->bipIndexOnBipAnimation;

                                    if(bipindexfromcache >= 30)
                                    {
                                        LOGI("skeleton num too much!!!\n");
                                    }
                                    
                                    //skeleton num >= k
                                    weightarray[j*4 + k] = su->objVertexBlendInfo[vertexIndexT[j]]->weight[k];

                                    indexarray[j*4 + k] = (float)bipindexfromcache;                                
         
                                }
                                else
                                {
                                    //skeleton num < k, and < 4
                                    weightarray[j*4 + k] = 0.0;

                                    indexarray[j*4 + k] = 0.0;
                                }
                            }
                        }

                        surface->setSkeletonWeight(weightarray);
                        surface->setSkeletonIndex(indexarray);
                       
                    }
                }
            }
        }
        }

        
		return 0;
	}
	

};

SE_Scene::SE_Scene(const SE_StringID& sceneName, SE_SCENE_TYPE t)
{
	mSceneRoot = NULL;
	mSelectedSpatial = NULL;
    mSpatialIDMap = new SE_Scene::SpatialIDMap;
    mName = sceneName;
    mType = t;
    mNeedDraw = true;
    mCamera = NULL;
    if(mType == SE_FRAMEBUFFER_SCENE)
    {
        mRenderTarget = new SE_FrameBufferTarget;
    }
    else
        mRenderTarget = NULL;
    mIsTranslucent = false;
	mVisibility = SE_VISIBLE;
	if(mType == SE_FRAMEBUFFER_SCENE)
	{
	    mCanHandleInput = true;
	}
	else
        mCanHandleInput = false;
}
SE_Scene::~SE_Scene()
{
    delete mSceneRoot;
    delete mSpatialIDMap;
    delete mCamera;
    delete mRenderTarget;
}
void SE_Scene::renderScene(SE_RenderManager& renderManager)
{
    if(mCamera)
    {
        mSceneRoot->renderScene(mCamera, &renderManager);
    }
    //renderManager.sort();
}
void SE_Scene::setMotionEventController(SE_MotionEventController* obj)
{
	if(mMotionEventController)
		delete mMotionEventController;
	mMotionEventController = obj;
}

void SE_Scene::releaseMotionEventController()
{
	if(mMotionEventController)
		delete mMotionEventController;
	mMotionEventController = NULL;
}
SE_MotionEventController* SE_Scene::getMotionEventController() const
{
	return mMotionEventController;
}
void SE_Scene::setKeyEventController(SE_KeyEventController* obj)
{
	if(mKeyEventController)
		delete mKeyEventController;
	mKeyEventController = obj;
}
void SE_Scene::releaseKeyEventController()
{
	if(mKeyEventController)
		delete mKeyEventController;
	mKeyEventController = NULL;
}
SE_KeyEventController* SE_Scene::getKeyEventController() const
{
	return mKeyEventController;
}
void SE_Scene::dispatchMotionEvent(SE_MotionEvent* motionEvent)
{
	if(mMotionEventController)
		mMotionEventController->onMotionEvent(motionEvent);
	else
	{
		if(mCamera)
		{
			mCamera->onMotionEvent(motionEvent);
		}
	}
}
void SE_Scene::dispatchKeyEvent(SE_KeyEvent* keyEvent)
{}
SE_Spatial* SE_Scene::getRoot()
{
    return mSceneRoot;
}
void SE_Scene::setRoot(SE_Spatial* root)
{
    if(mSceneRoot)
        delete mSceneRoot;
    mSceneRoot = root;
    mSceneRoot->mScene = this;
    updateSpatialIDMap();
}
void SE_Scene::setSelectedSpatial(SE_Spatial* spatial)
{
	if(mSelectedSpatial)
	{
		mSelectedSpatial->setSelected(false);
	}
	mSelectedSpatial = spatial;
	if(mSelectedSpatial)
	    mSelectedSpatial->setSelected(true);
}
SE_Spatial* SE_Scene::find(const SE_SpatialID& spatialID)
{
	SE_ASSERT(mSpatialIDMap);
	SE_Scene::SpatialIDMap::Map::iterator it = mSpatialIDMap->spatialIDMap.find(spatialID);
	if(it != mSpatialIDMap->spatialIDMap.end())
		return it->second;
	else
		return NULL;
}
void SE_Scene::setRenderTarget(SE_RenderTarget* renderTarget)
{
    if(mRenderTarget)
        delete mRenderTarget;
    mRenderTarget = renderTarget;
}
void SE_Scene::setCamera(SE_Camera* camera) 
{
    if(mCamera)
        delete mCamera;
    mCamera = camera;
}
void SE_Scene::createRoot(const char* sceneFileName)
{
	if(mSceneRoot != NULL)
	{
        //add new cbf to the exist scene
        delete mSceneRoot;	
	}
	mSceneRoot = SE_Application::getInstance()->getResourceManager()->loadScene(sceneFileName);
    mSceneRoot->mScene = this;
	updateSpatialIDMap();
}
void SE_Scene::updateSpatialIDMap()
{
    mSpatialIDMap->clear();
    _SpatialIDTravel addSpatialIDTravel;
	addSpatialIDTravel.spatialIDMap = mSpatialIDMap;
	addSpatialIDTravel.op = _SpatialIDTravel::ADD_SPATIAL;
	if(mSceneRoot)
	    mSceneRoot->travel(&addSpatialIDTravel, true);    
}

void SE_Scene::setSkeletonState()
{
    _setBoneState setBoneStateTravel;
    if(mSceneRoot)
	    mSceneRoot->travel(&setBoneStateTravel, true);

}

void SE_Scene::addSpatial(SE_Spatial* parent, SE_Spatial* child)
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
SE_Spatial* SE_Scene::removeSpatial(const SE_SpatialID& spatialID)
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
void SE_Scene::checkSpatialIDMap()
{}

void SE_Scene::unLoadScene()
{
    //release this after resourceManager free;
    mSpatialIDMap->clear();
}
/////////////
SE_SceneManager::SE_SceneManager()
{
	mMainScene = NULL;
}
SE_SceneManager::~SE_SceneManager()
{}
void SE_SceneManager::pushFront(SE_SCENE_TYPE t, SE_Scene* scene)
{
    if(t < 0 || t >= SE_SCENE_TYPE_NUM)
        return;
    if(scene == NULL)
        return;
    _SceneList* sceneList = &mSceneListArray[t];
    sceneList->push_front(scene);
}
void SE_SceneManager::pushBack(SE_SCENE_TYPE t, SE_Scene* scene)
{
    if(t < 0 || t >= SE_SCENE_TYPE_NUM)
        return;
    if(scene == NULL)
        return;
    _SceneList* sceneList = &mSceneListArray[t];
    sceneList->push_back(scene);
}
void SE_SceneManager::removeScene(SE_Scene* scene, bool isRelease)
{
    if(scene == NULL)
        return;
    bool found = false;
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList::iterator it;
        _SceneList* sceneList = &mSceneListArray[i];
        for(it = sceneList->begin(); it != sceneList->end(); it++)
        {
            if(scene == *it)
            {
                found = true;
                break;
            }
        }
    }
    if(found)
    {
        if(isRelease)
        {
            delete scene;
        }
        else
        {
			mRemovedSceneListArray[scene->getType()].push_back(scene);
        }
    }
}
void SE_SceneManager::removeScene(const SE_StringID& sceneName, bool isRelease)
{
    _SceneList rmSceneList;
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList::iterator it;
        _SceneList* sceneList = &mSceneListArray[i];
        for(it = sceneList->begin(); it != sceneList->end(); it++)
        {
            if(sceneName == (*it)->getName())
            {
                rmSceneList.push_back(*it);
            }
        }
    }
    _SceneList::iterator it;
    for(it = rmSceneList.begin() ; it != rmSceneList.end() ; it++)
    {
        if(isRelease)
        {
            delete *it;
        }
        else
        {
            mRemovedSceneListArray[(*it)->getType()].push_back(*it);
        }
    }
}
SE_SceneManager::_SceneSet SE_SceneManager::getScene(const SE_StringID& sceneName)
{
    _SceneList sceneList;
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList::iterator it;
        _SceneList* sceneList = &mSceneListArray[i];
        for(it = sceneList->begin(); it != sceneList->end(); it++)
        {
            if(sceneName == (*it)->getName())
            {
                sceneList->push_back(*it);
            }
        }
    }
    _SceneSet retV(sceneList.size());
    copy(sceneList.begin(), sceneList.end(), retV.begin());
    return retV;
}
SE_Scene* SE_SceneManager::getScene(SE_SCENE_TYPE t, const SE_StringID& sceneName)
{
    if(t < 0 || t >= SE_SCENE_TYPE_NUM)
        return NULL;
    _SceneList* sceneList = &mSceneListArray[t];
    _SceneList::iterator it;
    for(it = sceneList->begin() ; it != sceneList->end() ; it++)
    {
        if((*it)->getName() == sceneName)
            return *it;
    }
    return NULL;
}
void SE_SceneManager::insert(SE_SCENE_TYPE t, int index, SE_Scene* scene)
{
    if(t < 0 || t >= SE_SCENE_TYPE_NUM)
        return;
    if(scene == NULL)
        return;
    _SceneList* sceneList = &mSceneListArray[t];
    _SceneList::iterator it = listElementRef(*sceneList, index);
    sceneList->insert(it, scene);
}
class _RemoveScene
{
public:
    bool operator()(SE_Scene* scene)
    {
        if(scene->getName() == name)
            return true;
        else
            return false;
    }
    SE_StringID name;
};
void SE_SceneManager::renderScene(int& index, _SceneList* sceneList, SE_RenderManager& renderManager)
{
    _SceneList::iterator it;
    for(it = sceneList->begin() ; it != sceneList->end() ; it++)
    {
        SE_Scene* scene = *it;
        render(index, scene, renderManager);
    }
}
void SE_SceneManager::render(int& index , SE_Scene* scene, SE_RenderManager& renderManager)
{
	if(scene->needDraw() && scene->getVisibility() == SE_Scene::SE_VISIBLE)
    {
        bool ret = renderManager.setCurrentScene(index);
        if(ret)
        {
            renderManager.setCurrentRenderTarget(scene->getRenderTarget());
            renderManager.setCurrentCamera(scene->getCamera());
            renderManager.setCurrentBackgroundColor(scene->getBackgroundColor());
            scene->renderScene(renderManager);
            index++;
        }
        else
        {
            LOGI("set current scene failed\n");
        }
    }
}
void SE_SceneManager::renderFrameBufferScene(int& index, _SceneList* sceneList, SE_RenderManager& renderManager)
{
    _SceneList renderScene;
    _SceneList::reverse_iterator it;
    _SceneList::reverse_iterator firstOpaqueSceneIt = sceneList->rend();
    for(it = sceneList->rbegin() ; it != sceneList->rend() ; it++)
    {
        SE_Scene* scene = *it;
		if(!scene->isTranslucent() && scene->getVisibility() == SE_Scene::SE_VISIBLE)
        {
            firstOpaqueSceneIt = it;
            break; 
        }
    }
    if(firstOpaqueSceneIt == sceneList->rend())
    {
        _SceneList::iterator it1;
        for(it1 = sceneList->begin() ; it1 != sceneList->end() ; it1++)
        {
            SE_Scene* scene = *it;
            render(index, scene, renderManager);
        }
    }
    else
    {
        firstOpaqueSceneIt++;
        for(it = sceneList->rbegin() ; it != firstOpaqueSceneIt ; it++)
        {
            SE_Scene* scene = *it;
            renderScene.push_front(scene);
        }
        _SceneList::iterator it1;
        for(it1 = renderScene.begin() ; it1 != renderScene.end() ; it1++)
        {
            SE_Scene* scene = *it1;
            render(index, scene, renderManager);
        }
    }
}
bool SE_SceneManager::isPostEffectSceneTranslucent() 
{
    _SceneList* sceneList = &mSceneListArray[SE_POST_EFFECT_SCENE];
    _SceneList::reverse_iterator it;
    _SceneList::reverse_iterator firstOpaqueSceneIt = sceneList->rend();
    for(it = sceneList->rbegin() ; it != sceneList->rend() ; it++)
    {
        SE_Scene* scene = *it;
		if(!scene->isTranslucent() && scene->getVisibility() == SE_Scene::SE_VISIBLE)
        {
            firstOpaqueSceneIt = it;
            break; 
        }
    }
    if(firstOpaqueSceneIt != sceneList->rend())
    {
        return false;
    }
    else
        return true;
}
void SE_SceneManager::renderScene(SE_RenderManager& renderManager)
{
    int index = 0;
    _SceneList* sceneList = &mSceneListArray[SE_TEXTURE_SCENE];
    renderScene(index, sceneList, renderManager);
 
    sceneList = &mSceneListArray[SE_RENDERBUFFER_SCENE];
    renderScene(index, sceneList, renderManager);
 
    if(isPostEffectSceneTranslucent())
    {
        _SceneList* sceneList = &mSceneListArray[SE_FRAMEBUFFER_SCENE];
        renderFrameBufferScene(index, sceneList, renderManager);
    }
 
    sceneList = &mSceneListArray[SE_POST_EFFECT_SCENE];
    renderFrameBufferScene(index, sceneList, renderManager);
}
SE_SceneManager::_SceneSet SE_SceneManager::getSceneFromRemovedList(const SE_StringID& sceneName, bool remove)
{
    _SceneList newSceneList;
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList* sceneList = &mRemovedSceneListArray[i];
        _SceneList::iterator it;
        for(it = sceneList->begin() ; it != sceneList->end() ; it++)
        {
            if((*it)->getName() == sceneName)
            {
                newSceneList.push_back(*it);
            }
        }
    }
    _SceneSet retV(newSceneList.size());
    copy(newSceneList.begin(), newSceneList.end(), retV.begin());
    if(remove)
    {
        _RemoveScene rs;
        rs.name = sceneName;
        for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
        {
            _SceneList* sceneList = &mRemovedSceneListArray[i];
            sceneList->remove_if(rs);
        }
    }
    return retV;
}
SE_Scene* SE_SceneManager::getSceneFromRemovedList(SE_SCENE_TYPE t, const SE_StringID& sceneName, bool remove)
{
    if(t < 0 || t >= SE_SCENE_TYPE_NUM)
        return NULL;
    _SceneList* sceneList = &mRemovedSceneListArray[t];
    _SceneList::iterator it;
    SE_Scene* ret = NULL;
    for(it = sceneList->begin() ; it != sceneList->end() ; it++)
    {
        if((*it)->getName() == sceneName)
        {
            ret = *it;
            break;
        }
    }
    _RemoveScene rs;
    rs.name = sceneName;
    sceneList->remove_if(rs);
    return ret;
}
void SE_SceneManager::clear()
{
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList* sceneList = &mSceneListArray[i];
        _SceneList* removeList = &mRemovedSceneListArray[i];
        for_each(sceneList->begin(), sceneList->end(), SE_DeleteObject());
        for_each(removeList->begin(), removeList->end(), SE_DeleteObject());
    }
}
SE_Scene* SE_SceneManager::getTopScene()
{
	_SceneList* sceneList = &mSceneListArray[SE_POST_EFFECT_SCENE];
	_SceneList::reverse_iterator it;
	for(it = sceneList->rbegin() ; it != sceneList->rend() ; it++)
	{
		SE_Scene* scene = *it;
		if(scene->getVisibility() == SE_Scene::SE_VISIBLE && scene->canHandleInput())
		{
			return *it;
		}
	}
	sceneList = &mSceneListArray[SE_FRAMEBUFFER_SCENE];
    for(it = sceneList->rbegin() ; it != sceneList->rend() ; it++)
	{
		SE_Scene* scene = *it;
		if(scene->getVisibility() == SE_Scene::SE_VISIBLE && scene->canHandleInput())
		{
			return *it;
		}
	}
	return NULL;
}
SE_Spatial* SE_SceneManager::find(const SE_SpatialID& spatialID)
{
    for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList* sceneList = &mSceneListArray[i];
        _SceneList::iterator it;
        for(it = sceneList->begin() ; it != sceneList->end() ; it++)
        {
            SE_Scene* scene = *it;
            SE_Spatial* s = scene->find(spatialID);
            if(s != NULL)
                return s;
        }
    }
    return NULL;
}
SE_Spatial* SE_SceneManager::removeSpatial(const SE_SpatialID& spatialID)
{
	for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList* sceneList = &mSceneListArray[i];
        _SceneList::iterator it;
        for(it = sceneList->begin() ; it != sceneList->end() ; it++)
        {
            SE_Scene* scene = *it;
		    SE_Spatial* s = scene->removeSpatial(spatialID);
			if(s != NULL)
				return s;
        }
    }
    return NULL;
}
class _ReleaseVbo :public SE_SpatialTravel
{
public:
    virtual ~_ReleaseVbo(){}
    int visit(SE_Spatial* spatial)
	{		
		return 0;
	}
    int visit(SE_SimObject* simObject)
    {
        int surfaceCount = simObject->getMesh()->getSurfaceNum();
        for(int i = 0; i < surfaceCount; ++i)
        {
            for(int j = 0; j < (int)SE_Surface::VBO_TYPECOUNT;++j)
            {
                GLuint vboid = simObject->getMesh()->getSurface(i)->getVboID((SE_Surface::vboType)j);
                if(vboid != 0)
                {
                    glDeleteBuffers(1,&vboid);
                    LOGI("Release vbo success!!\n");
                }
            }

        }
        return 0;
    }
};
void SE_SceneManager::releaseVBO()
{
	for(int i = 0 ; i < SE_SCENE_TYPE_NUM ; i++)
    {
        _SceneList* sceneList = &mSceneListArray[i];
        _SceneList::iterator it;
        for(it = sceneList->begin() ; it != sceneList->end() ; it++)
        {
            SE_Scene* scene = *it;
		    SE_Spatial* s = scene->getRoot();
			_ReleaseVbo relaseVbo;
            s->travel(&relaseVbo,true);
        }
    }
}
void SE_SceneManager::setMainScene(const SE_StringID& name)
{
	SE_Scene* scene = getScene(SE_FRAMEBUFFER_SCENE, name);
	if(scene)
	{
		mMainScene = scene;
	}
}
