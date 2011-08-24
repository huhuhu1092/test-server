#include "SE_LoadThread.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_SceneManager.h"
#include "SE_SimObjectManager.h"
#include "SE_Spatial.h"
#include "SE_RenderState.h"
#include "SE_CChess.h"
#include "SE_Log.h"
SE_LoadThread::SE_LoadThread(SE_ResourceManager* rs, SE_CChess*app)
{
    resourceManager = rs;
    chessApp = app;
}
void SE_LoadThread::run()
{
    std::string dataPath = resourceManager->getDataPath();
    LOGI("### data path = %s ####", dataPath.c_str());
    if(dataPath != "")
    {
        LOGI("##### begin load resource #######");
        resourceManager->loadShader("ShaderDefine.xml");
	    resourceManager->loadRenderer("RendererDefine.xml");
	    resourceManager->loadFont("fontDefine.xml");
 	    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	    sceneManager->setWidth(480);
	    sceneManager->setHeight(800);
        chessApp->setBound(480, 800);
        chessApp->loadBoard();
	    //SE_Application::getInstance()->setState(SE_Application::RUNNING);

        SE_ResourceReadyCommand* rd = new SE_ResourceReadyCommand(SE_Application::getInstance());
        SE_Application::getInstance()->postCommand(rd); 
        LOGI("####### load resource end ############");
    }

}
SE_ResourceReadyCommand::SE_ResourceReadyCommand(SE_Application* app) : SE_Command(app)
{}
void SE_ResourceReadyCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    mApp->setState(SE_Application::RUNNING);
    LOGI("#### set app running ####\n");
}

