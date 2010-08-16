#include "SE_RenderManager.h"
#include "SE_RenderUnit.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_Geometry3D.h"
#include "SE_Log.h"
#include "SE_ShaderProgram.h"
#include "SE_ResourceManager.h"
#include <string.h>
static void checkGLError()
{
	/*
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
	*/
}
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
    glViewport(0, 0, rect.right - rect.left, rect.bottom - rect.top);
    checkGLError();
#ifdef DEBUG
    LOGI("## view port = %d, %d\n", rect.right - rect.left, rect.bottom - rect.top);
#endif
	SE_ShaderProgram* shaderProgram = SE_Application::getInstance()->getResourceManager()->getShaderProgram("main_vertex_shader");
    shaderProgram->use();
	glClearColor(1.0, 0, 0, 0);
	checkGLError();
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	checkGLError();
    glEnable(GL_DEPTH_TEST);
	checkGLError();
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
    for(int i = 0 ; i < RQ_NUM ; i++)
    {
        RenderUnitList* ruList = mRenderQueue[i];
        RenderUnitList::iterator it;
		int j = 0;
        for(it = ruList->begin() ; it != ruList->end() ;it++)
        {
            SE_RenderUnit* ru = *it;
			ru->setViewToPerspectiveMatrix(m);
#ifdef DEBUG
			LOGI("### draw %d ###\n", j++);
#endif
			//if(j >= 130)
            ru->draw();
        }

    }
}
void SE_RenderManager::addRenderUnit(SE_RenderUnit* ru, RENDER_QUEUE rq)
{
    mRenderQueue[rq]->push_back(ru);
}

