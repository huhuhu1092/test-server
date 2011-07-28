#include "SE_LoadThread.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_SceneManager.h"
#include "SE_SimObjectManager.h"
#include "SE_Spatial.h"
#include "SE_RenderState.h"
#include "SE_Log.h"
#include "SE_MotionEventCamera.h"
SE_LoadThread::SE_LoadThread(SE_ResourceManager* rs, const std::string& f)
{
    resourceManager = rs;
    fileName = f;
}
void SE_LoadThread::run()
{
    std::string dataPath = resourceManager->getDataPath();
    LOGI("### data path = %s ####", dataPath.c_str());
    if(dataPath != "")
    {
        LOGI("##### begin load resource #######");
	    resourceManager->loadBaseData(fileName.c_str()); 
        SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
		SE_Scene* scene = new SE_Scene(fileName.c_str());
		SE_Camera* camera = new SE_MotionEventCamera();
	    scene->setCamera(camera);
	    scene->createRoot(fileName.c_str());
		sceneManager->pushBack(scene->getType(), scene);
        SE_Spatial* rootScene = scene->getRoot();
        rootScene->updateWorldTransform();
	    rootScene->updateBoundingVolume();
		rootScene->updateWorldLayer();
		SE_DepthTestState* rs = new SE_DepthTestState();
		rs->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_ENABLE);
		rootScene->setRenderState(SE_Spatial::DEPTHTESTSTATE, rs, OWN);
		rootScene->updateRenderState();

        //SE_BlendState *rs_blend = new SE_BlendState();
        //rs_blend->setBlendProperty(SE_BlendState::BLEND_ENABLE);
        //rs_blend->setBlendDstFunc(SE_BlendState::ZERO);
        //rs_blend->setBlendSrcFunc(SE_BlendState::ONE);
        //rootScene->setRenderState(SE_Spatial::BLENDSTATE,rs_blend,OWN);
        //rootScene->updateRenderState();

		sceneManager->setMainScene(fileName.c_str());
        SE_ResourceReadyCommand* rd = new SE_ResourceReadyCommand(SE_Application::getInstance());
        SE_Application::getInstance()->postCommand(rd);
		

        //mApp->setCamera(SE_Application::MAIN_CAMERA, camera);
        //mApp->setCurrentCamera(SE_Application::MAIN_CAMERA);
        //SE_InputManager* inputManager = mApp->getInputManager();
        //inputManager->removeMotionEventObserver(NULL);     
        //inputManager->addMotionEventOberver(camera);//(new SE_MotionEventController());
#ifdef ANDROID
        SE_Application::getInstance()->sendMessageToJava("SE_LoadThread", "loadfinish"); 
#endif
        LOGI("####### load resource end ############\n");
    }

}
SE_ResourceReadyCommand::SE_ResourceReadyCommand(SE_Application* app) : SE_Command(app)
{}
void SE_ResourceReadyCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    mApp->setState(SE_Application::RUNNING);
    LOGI("#### set app running ####");
}

