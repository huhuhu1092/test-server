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

SE_RenderManager::SE_RenderManager()
{
    for(int i = 0 ;  i < SE_MAX_RENDERSCENE_SIZE ; i++)
    {
        _SceneRenderUnit* sru = new _SceneRenderUnit;
        mSceneRenderUnit[i] = sru;
    }
}
static bool _CompareRenderUnit(SE_RenderUnit* left, SE_RenderUnit* right)
{
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

void SE_RenderManager::sort()
{
    for(int i = 0 ;  i < SE_MAX_RENDERSCENE_SIZE ; i++)
    {
        _SceneRenderUnit* sru = mSceneRenderUnit[i];
		sru->renderTargetUnit.sort(SE_RenderManager::CompareRenderTarget);
	    std::list<_RenderTargetUnit*>::iterator it;
	    for(it = sru->renderTargetUnit.begin() ; it != sru->renderTargetUnit.end() ; it++)
	    {
		    _RenderTargetUnit* rt = *it;
            for(int i = 0 ; i < RQ_NUM ; i++)
            {
                RenderUnitList* ruList = rt->mRenderQueue[i];
                ruList->sort(_CompareRenderUnit) ;
            }
	    }
    }
}
void SE_RenderManager::clear()
{
}
SE_RenderManager::~SE_RenderManager()
{
    clear();
}
/*
void SE_RenderManager::beginDraw()
{
}
*/
void SE_RenderManager::endDraw()
{
    clear();
}
void SE_RenderManager::draw()
{
    SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    for(int i = 0 ; i < SE_MAX_RENDERSCENE_SIZE ; i++)
    {
        _SceneRenderUnit* sru = mSceneRenderUnit[i];
        if(!sru->needDraw)
            continue;
        std::list<_RenderTargetUnit*>::iterator it;
        for(it = sru->renderTargetUnit.begin() ; it != sru->renderTargetUnit.end() ; it++)
        {
            _RenderTargetUnit* rt = *it;
            SE_RenderTarget* renderTarget = renderTargetManager->getRenderTarget(rt->mRenderTargetID);
            SE_Camera* camera = renderTarget->getCamera();
		    if(renderTarget->prepare() && camera)
		    {
				SE_Matrix4f m = camera->getPerspectiveMatrix().mul(camera->getWorldToViewMatrix());
				SE_Rect<int> rect = camera->getViewport();
			    SE_Renderer::setViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
                if(renderTarget->getClearTarget())
                {
				    SE_Renderer::setClearColor(renderTarget->getBackground());
			        SE_Renderer::clear(SE_Renderer::SE_COLOR_BUFFER | SE_Renderer::SE_DEPTH_BUFFER);
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
	//for(; it != mRenderTargetList.end() ; it++)
	while(it != mRenderTargetList.end())
	{
		_RenderTargetUnit* rt = *it;
		SE_RenderTarget* renderTarget = renderTargetManager->getRenderTarget(rt->mRenderTargetID);
		SE_Camera* camera = renderTarget->getCamera();
		if(renderTarget->prepare())
		{
			if(camera)
			{
				m = camera->getPerspectiveMatrix().mul(camera->getWorldToViewMatrix());
				SE_Rect<int> rect = camera->getViewport();
			    SE_Renderer::setViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
				SE_Renderer::setClearColor(renderTarget->getBackground());
			    SE_Renderer::clear(SE_Renderer::SE_COLOR_BUFFER | SE_Renderer::SE_DEPTH_BUFFER);
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
		it++;
	}
	if(startIt != mRenderTargetList.end())
	{
        _RenderTargetUnit* rt = *startIt;
		SE_RenderTarget* renderTarget = renderTargetManager->getRenderTarget(rt->mRenderTargetID);
		if(renderTarget->prepare())
		{
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
	}
    */
}
SE_RenderManager::_RenderTargetUnit* SE_RenderManager::findTarget(_SceneRenderUnit* sceneRenderUnit, const SE_RenderTargetID& id)
{
	std::list<_RenderTargetUnit*>::iterator it;
	for(it = sceneRenderUnit->renderTargetUnit.begin() ; it != sceneRenderUnit->renderTargetUnit.end() ; it++)
	{
        _RenderTargetUnit* rt = *it;
		if(rt->mRenderTargetID == id)
		{
			return rt;
		}
	}
	return NULL;
}
void SE_RenderManager::addRenderUnit(SE_RenderUnit* ru, const SE_SceneRenderSeq& sceneRenderSeq, const SE_RenderTargetID& renderTarget, RENDER_QUEUE rq)
{
    SE_ASSERT(sceneRenderSeq >= 0 && sceneRenderSeq < SE_MAX_RENDERSCENE_SIZE);
    _SceneRenderUnit* sceneRenderUnit = mSceneRenderUnit[sceneRenderSeq.toInt()];
    _RenderTargetUnit* rt = findTarget(sceneRenderUnit, renderTarget);
	if(rt == NULL)
	{
	    rt = new _RenderTargetUnit;
	    rt->mRenderTargetID = renderTarget;
	    sceneRenderUnit->renderTargetUnit.push_back(rt);
	}
    rt->mRenderQueue[rq]->push_back(ru);
}

