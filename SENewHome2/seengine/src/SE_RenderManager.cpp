#include "SE_RenderManager.h"
#include "SE_RenderUnit.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_Geometry3D.h"
#include "SE_Log.h"
#include "SE_ShaderProgram.h"
#include "SE_ResourceManager.h"
#include "SE_Layer.h"
#include "SE_Renderer.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
#include <string.h>
#include "SE_MemLeakDetector.h"
SE_RenderManager::SE_RenderManager()
{
	mCurrentScene = -1;
	mRenderSortType = SORT_BY_DISTANCE;
}
struct _CompareRenderUnit
{
	SE_RenderManager::RENDER_SORT_TYPE type;
    bool operator()(SE_RenderUnit* left, SE_RenderUnit* right)
	{
		if(type == SE_RenderManager::SORT_BY_DISTANCE)
		{
		    if(left->getDistanceToCamera() < right->getDistanceToCamera())
			    return true;
		    else
			    return false;
		}
		else if(type == SE_RenderManager::SORT_BY_RESOURCE)
		{
			SE_ProgramDataID leftProgramID = left->getShaderProgramID();
			SE_ProgramDataID rightProgramID = right->getShaderProgramID();
			SE_ImageDataID* leftImageDataArray = NULL;
			int leftImageDataArrayNum = 0;
			SE_ImageDataID* rightImageDataArray = NULL;
			int rightImageDataArrayNum = 0;
			const SE_Layer& leftLayer = left->getLayer();
			const SE_Layer& rightLayer = right->getLayer();
			if(leftLayer < rightLayer)
				return true;
			if(leftLayer > rightLayer)
				return false;
			if(leftProgramID < rightProgramID)
				return true;
			if(leftProgramID > rightProgramID)
				return false;
			left->getTexImageID(SE_TEXTURE0,leftImageDataArray, leftImageDataArrayNum);
			right->getTexImageID(SE_TEXTURE0,rightImageDataArray, rightImageDataArrayNum);
			if(leftImageDataArray == NULL && rightImageDataArray != NULL)
				return true;
			if(leftImageDataArray != NULL && rightImageDataArray == NULL)
				return false;
			if(leftImageDataArray == NULL && rightImageDataArray == NULL)
			{
				SE_ImageData** leftBaseColorImage = NULL;
				int leftBaseColorImageNum;
				SE_ImageData** rightBaseColorImage = NULL;
				int rightBaseColorImageNum;
				left->getTexImage(SE_TEXTURE0, leftBaseColorImage, leftBaseColorImageNum);
				right->getTexImage(SE_TEXTURE0, rightBaseColorImage, rightBaseColorImageNum);
				if(leftBaseColorImageNum > 0 && rightBaseColorImageNum == 0)
					return false;
				if(leftBaseColorImageNum == 0 && rightBaseColorImageNum > 0)
					return true;
				if(leftBaseColorImageNum == 0 && rightBaseColorImageNum == 0)
					return false;
				SE_ImageData* leftImageData = leftBaseColorImage[0];
				SE_ImageData* rightImageData = rightBaseColorImage[0];
				return leftImageData < rightImageData;
			}
			if(leftImageDataArray[0] < rightImageDataArray[0])
				return true;
			else
				return false;
		}
	}
};
void SE_RenderManager::sort()
{
	_CompareRenderUnit cru;
	cru.type = mRenderSortType;
    for(int i = 0 ; i < SE_MAX_SCENE_SIZE ; i++)
    {
        _SceneUnit* sceneUnit = &mSceneUnits[i];
		if(sceneUnit->mRenderTarget == NULL || sceneUnit->mCamera == NULL)
			continue;
        for(int j = 0 ; j < RQ_NUM ; j++)
        {
            RenderUnitList* ruList = &sceneUnit->mRenderQueue[j];
            ruList->sort(cru);
        } 
    }
}
void SE_RenderManager::clear()
{
    for(int i = 0 ; i < SE_MAX_SCENE_SIZE ; i++)
    {
        _SceneUnit* sceneUnit = &mSceneUnits[i];
        sceneUnit->mCamera = NULL;
        sceneUnit->mRenderTarget = NULL;
        for(int j = 0 ; j < RQ_NUM ; j++)
        {
            RenderUnitList* ruList = &sceneUnit->mRenderQueue[j];
            RenderUnitList::iterator it;
			for(it = ruList->begin() ; it != ruList->end() ;it++)
			{
				SE_RenderUnit* ru = *it;
				delete ru;
			}
			ruList->clear();
        }
    }
    /*
	std::list<_RenderTargetUnit*>::iterator it;
	for(it = mRenderTargetList.begin() ; it != mRenderTargetList.end() ; it++)
	{
		_RenderTargetUnit* rt = *it;
		if(rt)
		{
			for(int i = 0 ; i  < RQ_NUM ; i++)
			{
				RenderUnitList* ruList = rt->mRenderQueue[i];
				RenderUnitList::iterator it;
				for(it = ruList->begin() ; it != ruList->end() ;it++)
				{
					SE_RenderUnit* ru = *it;
					delete ru;
				}
				ruList->clear();
				delete ruList;
			}
			delete rt;
		}
	}
    mRenderTargetList.clear();
    */
}
SE_RenderManager::~SE_RenderManager()
{
    clear();
}
void SE_RenderManager::beginDraw()
{
    clear();
	SE_Application::getInstance()->getStatistics().clear();
}
void SE_RenderManager::endDraw()
{}

void SE_RenderManager::draw()
{
#ifdef DEBUG
	int renderUnitNum = 0;
#endif
    for(int i = 0 ; i < SE_MAX_SCENE_SIZE ; i++)
    {
        _SceneUnit* sceneUnit = &mSceneUnits[i];
        SE_RenderTarget* renderTarget = sceneUnit->mRenderTarget;
        SE_Camera* camera = sceneUnit->mCamera;
        if(renderTarget == NULL || camera == NULL)
        {
            //LOGI("render target is null or camera is null, not draw this scene\n");
            continue;
        }
        if(!renderTarget->prepare())
        {
            //LOGI("render target prepare failed\n");
            continue;
        }
        SE_Matrix4f m = camera->getPerspectiveMatrix().mul(camera->getWorldToViewMatrix());
		SE_Rect<int> rect = camera->getViewport();
		if((rect.right - rect.left) == 0 || (rect.bottom - rect.top) == 0)
			continue;
		SE_Renderer::setViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
        if(renderTarget->isClearTargetColor())
        {
			SE_Renderer::setClearColor(sceneUnit->mBgColor);
			SE_Renderer::clear(SE_Renderer::SE_COLOR_BUFFER | SE_Renderer::SE_DEPTH_BUFFER);
        }
        for(int j = 0 ; j < RQ_NUM ; j++)
        {
			RenderUnitList* ruList = &sceneUnit->mRenderQueue[j];
			RenderUnitList::iterator it;
			for(it = ruList->begin() ; it != ruList->end(); it++)
			{
				SE_RenderUnit* ru = *it;
				ru->setViewToPerspectiveMatrix(m);
				ru->applyRenderState();
				ru->draw();
#ifdef DEBUG
		        renderUnitNum++;
#endif
            }
        }
    }
    /*
	if(mRenderTargetList.empty())
		return;
    SE_Matrix4f m;
	std::list<_RenderTargetUnit*>::iterator it = mRenderTargetList.begin();
	std::list<_RenderTargetUnit*>::iterator startIt = mRenderTargetList.begin();
	it++;
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	for(; it != mRenderTargetList.end() ; it++)
	{
		_RenderTargetUnit* rt = *it;
		SE_RenderTarget* renderTarget = renderTargetManager->getRenderTarget(rt->mRenderTargetID);
		SE_Camera* camera = renderTarget->getCamera();
        if(camera)
		{
			m = camera->getPerspectiveMatrix().mul(camera->getWorldToViewMatrix());
		}
		for(int i = 0 ; i < RQ_NUM ; i++)
		{
			RenderUnitList* ruList = rt->mRenderQueue[i];
			RenderUnitList::iterator it;
			for(it = ruList->begin() ; it != ruList->end() ;it++)
			{
				SE_RenderUnit* ru = *it;
				ru->setViewToPerspectiveMatrix(m);
				ru->applyRenderState();
				ru->draw();
			}
		}
	}
	if(startIt != mRenderTargetList.end())
	{
        _RenderTargetUnit* rt = *startIt;
		SE_Camera* currCamera = SE_Application::getInstance()->getCurrentCamera();
        SE_Rect<int> rect = currCamera->getViewport();
	    SE_Renderer::setViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
	    SE_Renderer::setClearColor(SE_Vector4f(mBackground.x, mBackground.y, mBackground.z, 1.0));
	    SE_Renderer::clear(SE_Renderer::SE_COLOR_BUFFER | SE_Renderer::SE_DEPTH_BUFFER);
		m = mPerspectiveMatrix.mul(mWorldToViewMatrix);
		for(int i = 0 ; i < RQ_NUM ; i++)
		{
			RenderUnitList* ruList = rt->mRenderQueue[i];
			RenderUnitList::iterator it;
			for(it = ruList->begin() ; it != ruList->end() ;it++)
			{
				SE_RenderUnit* ru = *it;
				ru->setViewToPerspectiveMatrix(m);
				ru->applyRenderState();
				ru->draw();
			}
		}
	}
    */
}
/*
SE_RenderManager::_RenderTargetUnit* SE_RenderManager::findTarget(const SE_RenderTargetID& id)
{
	std::list<_RenderTargetUnit*>::iterator it;
	for(it = mRenderTargetList.begin() ; it != mRenderTargetList.end() ; it++)
	{
        _RenderTargetUnit* rt = *it;
		if(rt->mRenderTargetID == id)
		{
			return rt;
		}
	}
	return NULL;

}
*/
void SE_RenderManager::addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq)
{
    _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    RenderUnitList* ruList = &sceneUnit->mRenderQueue[rq];
    ruList->push_back(ru);
    /*
    _RenderTargetUnit* rt = findTarget(renderTarget);
	if(rt == NULL)
	{
	    rt = new _RenderTargetUnit;
	    rt->mRenderTargetID = renderTarget;
	    mRenderTargetList.push_back(rt);
	}
    rt->mRenderQueue[rq]->push_back(ru);
    */
}
void SE_RenderManager::setCurrentRenderTarget(SE_RenderTarget* renderTarget)
{
    _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    sceneUnit->mRenderTarget = renderTarget;
}
SE_RenderTarget* SE_RenderManager::getCurrentRenderTarget() const
{
    const _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    return sceneUnit->mRenderTarget;
}
void SE_RenderManager::setCurrentCamera(SE_Camera* camera)
{
    _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    sceneUnit->mCamera = camera;
}
SE_Camera* SE_RenderManager::getCurrentCamera() const
{
    const _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    return sceneUnit->mCamera;
}
void SE_RenderManager::setCurrentBackgroundColor(const SE_Vector4f& c)
{
    _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    sceneUnit->mBgColor = c;
}
SE_Vector4f SE_RenderManager::getCurrentBackgroundColor() const
{
    const _SceneUnit* sceneUnit = &mSceneUnits[mCurrentScene];
    return sceneUnit->mBgColor;
}
