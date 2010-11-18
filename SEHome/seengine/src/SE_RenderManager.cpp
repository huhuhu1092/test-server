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
#include <string.h>

SE_RenderManager::SE_RenderManager()
{
    
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        mRenderQueue[i] = new RenderUnitList;
    }
    /*
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        mRenderQueue[i] = NULL;
    }
    */
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
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        ruList->sort(_CompareRenderUnit) ;
    }
}
SE_RenderManager::~SE_RenderManager()
{
    for(int i = 0 ; i  < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        RenderUnitList::iterator it;
        for(it = ruList->begin() ; it != ruList->end() ;it++)
        {
            SE_RenderUnit* ru = *it;
            delete ru;
        }
        ruList->clear();
        delete ruList;
    }

}
void SE_RenderManager::beginDraw()
{
     SE_Camera* currCamera = SE_Application::getInstance()->getCurrentCamera();
    SE_Rect<int> rect = currCamera->getViewport();
	SE_Renderer::setViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
#ifdef DEBUG0
    LOGI("## view port = %d, %d\n", rect.right - rect.left, rect.bottom - rect.top);
	SE_Vector3f location = currCamera->getLocation();
	LOGI("## location = %f, %f, %f\n", location.x, location.y, location.z);
#endif
	//SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram("main_shader");
    //shaderProgram->use();
	SE_Renderer::setClearColor(SE_Vector4f(mBackground.x, mBackground.y, mBackground.z, 1.0));
	SE_Renderer::clear(SE_Renderer::SE_COLOR_BUFFER | SE_Renderer::SE_DEPTH_BUFFER);
    //glEnable(GL_DEPTH_TEST);
	//glDisable(GL_DEPTH_TEST);
	//glEnable(GL_BLEND);
	//glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    for(int i = 0 ; i  < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        RenderUnitList::iterator it;
        for(it = ruList->begin() ; it != ruList->end() ;it++)
        {
            SE_RenderUnit* ru = *it;
            delete ru;
        }
        ruList->clear();
    }
}
void SE_RenderManager::endDraw()
{}
void SE_RenderManager::draw()
{
    SE_Matrix4f m = mPerspectiveMatrix.mul(mWorldToViewMatrix);
#ifdef DEBUG0
	int j = 0;
#endif
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        RenderUnitList::iterator it;
        for(it = ruList->begin() ; it != ruList->end() ;it++)
        {
            SE_RenderUnit* ru = *it;
			ru->setViewToPerspectiveMatrix(m);
			ru->applyRenderState();
			//if(j >= 130)
            ru->draw();
#ifdef DEBUG0
			j++;
#endif
        }

    }
#ifdef DEBUG0
	LOGI("### draw %d ###\n", j);
#endif
}
void SE_RenderManager::addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq)
{
    mRenderQueue[rq]->push_back(ru);
}

