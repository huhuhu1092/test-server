#include "SE_SystemCommand.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SceneManager.h"
#include "SE_Ase.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Camera.h"
#include "SE_Quat.h"
#include "SE_CommonNode.h"
#include "SE_Geometry3D.h"
#include "SE_SpatialTravel.h"
#include "SE_InputManager.h"
#include "SE_InputEvent.h"
#include "SE_MotionEventCamera.h"
#include "SE_MotionEventController.h"
#include "SE_SceneManager.h"
#include "SE_UserCommand.h"
#include "SE_SimObjectManager.h"
#include "SE_ThreadManager.h"
#include "SE_LoadThread.h"
#include "SE_Log.h"
#include "SE_MemLeakDetector.h"
SE_InitAppCommand::SE_InitAppCommand(SE_Application* app) : SE_Command(app)
{}
SE_InitAppCommand::~SE_InitAppCommand()
{}
void SE_InitAppCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    std::string inPath = dataPath + SE_SEP + fileName + ".ASE";
    std::string outPath = dataPath + SE_SEP + fileName;
   
#if 0
    ASE_Loader loader;
    loader.Load(inPath.c_str(), 0);


    //inPath = dataPath + SE_SEP + "LOD0_clock.ase";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "LOD1_clock.ase";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "test.ASE";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + fileName + ".SEINFO";
    //loader.Load(inPath.c_str(), 0);
   

    //inPath = dataPath + SE_SEP + "phone_base.ASE";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "phone_receiver.ASE";
    //loader.Load(inPath.c_str(), 0);


    //inPath = dataPath + SE_SEP + "laptop.ASE";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "book.ASE";
    //loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "mesh.ASE";
    //loader.Load(inPath.c_str(), 0);
    //inPath = dataPath + SE_SEP + "camera.ASE";
//    loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "book.ASE";
    //loader.Load(inPath.c_str(), 0);
   

    //inPath = dataPath + SE_SEP + "bone.txt";
    //loader.Load(inPath.c_str(), 0);

    inPath = dataPath + SE_SEP + "rendererdefine.ASE";
    loader.Load(inPath.c_str(), 0);

    inPath = dataPath + SE_SEP + "shaderdefine.ASE";
    loader.Load(inPath.c_str(), 0);

    //inPath = dataPath + SE_SEP + "ttt.ASE";
    //loader.Load(inPath.c_str(), 0);
   
    loader.LoadEnd();
    loader.Write(dataPath.c_str(), outPath.c_str());
#endif 
   
   

    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->setDataPath(dataPath.c_str());
#if 0
	//please commet following code if thread bug is resolved
    if(dataPath != "")
    {
        resourceManager->loadBaseData(fileName.c_str());
        SE_SceneManager* sceneManager = mApp->getSceneManager();
        sceneManager->createScene(fileName.c_str());
        SE_Spatial* rootScene = sceneManager->getRoot();
        rootScene->updateWorldTransform();
        rootScene->updateBoundingVolume();
        SE_DepthTestState* rs = new SE_DepthTestState();
        rs->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_ENABLE);
        rootScene->setRenderState(SE_Spatial::DEPTHTESTSTATE, rs, OWN);
        rootScene->updateRenderState();

        SE_BlendState *rs_blend = new SE_BlendState();
        rs_blend->setBlendProperty(SE_BlendState::BLEND_ENABLE);
        rs_blend->setBlendDstFunc(SE_BlendState::ZERO);
        rs_blend->setBlendSrcFunc(SE_BlendState::ONE);
        rootScene->setRenderState(SE_Spatial::BLENDSTATE,rs_blend,OWN);
        rootScene->updateRenderState();
    }
    mApp->setState(SE_Application::RUNNING);
	////////////////////
	SE_Camera* camera = new SE_MotionEventCamera();
    mApp->setCamera(SE_Application::MAIN_CAMERA, camera);
    mApp->setCurrentCamera(SE_Application::MAIN_CAMERA);
    SE_InputManager* inputManager = mApp->getInputManager();
    inputManager->removeMotionEventObserver(NULL);     
    inputManager->addMotionEventOberver(camera);//(new SE_MotionEventController());
#endif
    //end

#if 1   
    SE_LoadThread* lthread = new SE_LoadThread(resourceManager, fileName);
    lthread->setName("loadresource");
    SE_ThreadManager* threadManager = SE_Application::getInstance()->getThreadManager();
    threadManager->add(lthread);
    lthread->start();
#endif
    //inputManager->addMotionEventOberver(new SE_MotionEventCamera());

   
}

////////////////////////////////////////////////////////

////////////////////////////////////////////////////////
SE_KeyEventCommand::SE_KeyEventCommand(SE_Application* app) : SE_Command(app)
{
    keyEvent = NULL;
}
SE_KeyEventCommand::~SE_KeyEventCommand()
{
    if(keyEvent)
        delete keyEvent;
}
void SE_KeyEventCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_InputManager* inputManager = mApp->getInputManager();
    inputManager->update(keyEvent);
}

SE_MotionEventCommand::SE_MotionEventCommand(SE_Application* app) : SE_Command(app)
{
    motionEvent = NULL;
}
SE_MotionEventCommand::~SE_MotionEventCommand()
{
    if(motionEvent)
        delete motionEvent;
}
void SE_MotionEventCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_InputManager* inputManager = mApp->getInputManager();
    inputManager->update(motionEvent);
}
//////////////
SE_LoadSceneCommand::SE_LoadSceneCommand(SE_Application* app) : SE_Command(app)
{}
void SE_LoadSceneCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    if(sceneName == "")
        return;
    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->loadBaseData(sceneName.c_str());
    SE_SceneManager* sceneManager = mApp->getSceneManager();
	SE_Scene* scene = new SE_Scene(sceneName.c_str());
    scene->createRoot(sceneName.c_str());
    SE_Spatial* rootScene = scene->getRoot();
    rootScene->updateWorldTransform();
    rootScene->updateBoundingVolume();   
	sceneManager->pushBack(scene->getType(), scene);
}

////////////////////////////////////////////////////////
/*
SE_MoveCameraCommand::SE_MoveCameraCommand(SE_Application* app) : SE_Command(app)
{
}
SE_MoveCameraCommand::~SE_MoveCameraCommand()
{}
void SE_MoveCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Vector3f startLocation = camera->getLocation();
    camera->rotateLocal(rotateAngle, axis);
    camera->translateLocal(translate);
    SE_Vector3f endLocation = camera->getLocation();
    SE_Sphere sphere;
    sphere.set(startLocation, 2);
    SE_SceneManager* sceneManager = mApp->getSceneManager();
    SE_Spatial* rootScene = sceneManager->getRoot();
    SE_MovingSphereStaticSpatialIntersect moveTravel(sphere, endLocation);
    rootScene->travel(&moveTravel, true);
    if(moveTravel.intersected)
    {
        camera->setLocation(moveTravel.location);
    }
}
*/
///////////////////////////////////camera ralates////////////////////
SE_InitCameraCommand::SE_InitCameraCommand(SE_Application* app) : SE_Command(app)
{}
SE_InitCameraCommand::~SE_InitCameraCommand()
{}
#if defined (WIN32)
void SE_InitCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
    SE_Vector3f location(80,0,93);
	SE_Vector3f zAxis(1, 0, 0);	
	SE_Vector3f up(0, 0, 1);
	SE_Camera* c = mApp->getCurrentCamera();
	c->create(location, zAxis, up, 60.0f,((float)height)/ width, 1.0f, 1000.0f);
    c->setViewport(0, 0, width, height);
}
#else
void SE_InitCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Camera* c = mApp->getCurrentCamera();
    c->create(mLocation, mAxisZ, mUp, mFov,((float)height)/ width, mNear, mFar);
    c->setViewport(0, 0, width, height);
}
#endif


#if defined (WIN32)
SE_UpdateCameraCommand::SE_UpdateCameraCommand(SE_Application* app) : SE_Command(app)
{}
SE_UpdateCameraCommand::~SE_UpdateCameraCommand()
{}
void SE_UpdateCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Camera* c = mApp->getCurrentCamera();
    if(c)
    {
        SE_Vector3f location = c->getLocation();
        SE_Vector3f zAxis = c->getAxisZ();
        SE_Vector3f up(0, 0, 1);        
        c->create(location, zAxis, up, 60.0f,((float)height)/ width, 1.0f, 1000.0f);
        c->setViewport(0, 0, width, height);  
    }
}
#else
SE_UpdateCameraCommand::SE_UpdateCameraCommand(SE_Application* app) : SE_Command(app)
{}
SE_UpdateCameraCommand::~SE_UpdateCameraCommand()
{}
void SE_UpdateCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Camera* c = mApp->getCurrentCamera();
    if(c)
    {
       SE_Vector3f location = c->getLocation();
       SE_Vector3f zAxis = c->getAxisZ();
       SE_Vector3f up = c->getAxisY();
       c->create(location, zAxis, up, mFov,((float)mHeight)/ mWidth, mNear, mFar);
       c->setViewport(0, 0, mWidth, mHeight);
    }
}
#endif

SE_SetCameraCommand::SE_SetCameraCommand(SE_Application* app) : SE_Command(app)
{
    mFlag = false;
}
SE_SetCameraCommand::~SE_SetCameraCommand()
{}
void SE_SetCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    if (mFlag) {
        mCamera->create(mLocation, mTarget, mFov, mRatio, mNear, mFar);
    } else {
    mCamera->create(mLocation, mAxisZ, mUp, mFov, mRatio, mNear, mFar);
    }
}

SE_SetViewportCommand::SE_SetViewportCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetViewportCommand::~SE_SetViewportCommand()
{}
void SE_SetViewportCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    mCamera->setViewport(mX, mY, mW, mH);
}

SE_SetFrustumCommand::SE_SetFrustumCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetFrustumCommand::~SE_SetFrustumCommand()
{}

void SE_SetFrustumCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	mCamera->setFrustum(mFov, mRatio, mNear, mFar);
}

SE_RotateLocalCommand_I::SE_RotateLocalCommand_I(SE_Application* app) : SE_Command(app)
{
}
SE_RotateLocalCommand_I::~SE_RotateLocalCommand_I()
{}
void SE_RotateLocalCommand_I::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    mCamera->rotateLocal(mRotate);
}

SE_RotateLocalCommand_II::SE_RotateLocalCommand_II(SE_Application* app) : SE_Command(app)
{
}
SE_RotateLocalCommand_II::~SE_RotateLocalCommand_II()
{}
void SE_RotateLocalCommand_II::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    LOGI("## rotate camera ###mAngle =%f,  mAxis=%d\n",mAngle, mAxis);
    mCamera->rotateLocal(mAngle, mAxis);
}

SE_TranslateLocalCommand::SE_TranslateLocalCommand(SE_Application* app) : SE_Command(app)
{
}
SE_TranslateLocalCommand::~SE_TranslateLocalCommand()
{}
void SE_TranslateLocalCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Vector3f startLocation = mCamera->getLocation();
    mCamera->translateLocal(mTranslate);
    SE_Vector3f endLocation = mCamera->getLocation();
    SE_Sphere sphere;
    sphere.set(startLocation, 5);
    SE_SceneManager* sceneManager = mApp->getSceneManager();
    SE_Spatial* rootScene = sceneManager->getMainScene()->getRoot();
    SE_MovingSphereStaticSpatialIntersect moveTravel(sphere, endLocation);
    rootScene->travel(&moveTravel, true);
    if(moveTravel.intersected)
    {
        mCamera->setLocation(moveTravel.location);
    }
}

SE_SetLocationCommand::SE_SetLocationCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetLocationCommand::~SE_SetLocationCommand()
{}
void SE_SetLocationCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    mCamera->setLocation(mLocation);
}
