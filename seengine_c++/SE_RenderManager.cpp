#include "SE_RenderManager.h"
#include "SE_RenderUnit.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_Geometry3D.h"
#include "SE_Log.h"
#include <string.h>
SE_RenderManager::SE_RenderManager()
{
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        mRenderQueue[i] = new RenderUnitList;
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
        delete ruList;
    }
}
void SE_RenderManager::beginDraw()
{
    SE_Camera* currCamera = SE_Application::getInstance()->getCurrentCamera();
    SE_Rect<int> rect = currCamera->getViewport();
    glViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
    LOGI("## view port = %d, %d\n", rect.right - rect.left, rect.bottom - rect.top);
}
void SE_RenderManager::endDraw()
{}
void SE_RenderManager::draw()
{
    SE_Matrix4f m = mPerspectiveMatrix.mul(mWorldToViewMatrix);
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        RenderUnitList::iterator it;
        for(it = ruList->begin() ; it != ruList->end() ;it++)
        {
            SE_RenderUnit* ru = *it;
            ru->setWorldTransform(m);
            ru->draw();
        }

    }
}
void SE_RenderManager::addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq)
{
    mRenderQueue[rq]->push_back(ru);
}

