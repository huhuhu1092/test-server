#include "SE_2DCommand.h"
#include "SE_UieManager.h"
#include "SE_Application.h"
#include "SE_SceneManager.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_CommonNode.h"
#include "SE_Primitive.h"
#include "SE_ImageCodec.h"
#include "SE_MotionEventCamera.h"
#include "SE_Geometry.h"
#include "SE_MeshSimObject.h"
#include "SE_ImageData.h"
#include "SE_InputManager.h"
#include "SE_IO.h"
#include "SE_ElementManager.h"
#include "SE_RenderState.h"
#include "SE_SceneManager.h"
#include "SE_SimObject.h"
#include "SE_AnimationManager.h"
#include "SE_Animation.h"
#include "SE_Spatial.h"
#include "SE_RenderManager.h"
#include "SE_ImageMap.h"
#include "SE_CheckXml.h"
#include "SE_Scene.h"
#include "SE_SceneManager.h"
#include "SE_CameraManager.h"
#include "SE_CChess.h"
#include "SE_ThreadManager.h"
#include "SE_LoadThread.h"
#include <math.h>
#include <wchar.h>
#include <string.h>
SE_Init2D::SE_Init2D(SE_Application* app) : SE_Command(app)
{
	left = top = width = height = 0;
}
SE_Init2D::~SE_Init2D()
{}

void SE_Init2D::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
 	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	sceneManager->setWidth(width);
	sceneManager->setHeight(height);
    chessApp->setBound(width, height);

    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->setDataPath(dataPath.c_str());
    
	resourceManager->loadShader("ShaderDefine.xml");
	resourceManager->loadRenderer("RendererDefine.xml");
	resourceManager->loadFont("fontDefine.xml");
    //chessApp->start();
    chessApp->loadBoard();
	SE_Application::getInstance()->setState(SE_Application::RUNNING);
	
	//begin
	/*
	SE_ThreadManager* threadManager = SE_GET_THREADMANAGER();
	SE_LoadThread* lthread = new SE_LoadThread(resourceManager, chessApp);
    lthread->setName("loadthread");
	threadManager->add(lthread);
	lthread->start();
    */
	//end
	//SE_NetAddress na("127.0.0.1", 5999);
    //SE_UieAcceptThread* uieAcceptThread = new SE_UieAcceptThread(na);
	//threadManager->add(uieAcceptThread);
	//uieAcceptThread->start();

}
//////////////
SE_2DUpdateCameraCommand::SE_2DUpdateCameraCommand(SE_Application* app) : SE_Command(app)
{
	width = 480;
	height = 800;
}
SE_2DUpdateCameraCommand::~SE_2DUpdateCameraCommand()
{}
void SE_2DUpdateCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	/*
    SE_Vector3f location(0, 0, 10);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Rect<int> viewport = elementManager->getViewport();
    float ratio = (viewport.bottom - viewport.top) / (float)(viewport.right - viewport.left);
	float angle = 2 * SE_RadianToAngle(atanf((viewport.right - viewport.left) / 20.0f));
    SE_Camera* camera = new SE_MotionEventCamera;
	camera->create(location, SE_Vector3f(0, 0, 1), SE_Vector3f(0, 1, 0), angle, ratio, 1, 50);//(location, SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), angle * 2, ratio, 1, 20);
	camera->setViewport(0, 0, viewport.right - viewport.left, viewport.bottom - viewport.top);
	//camera->setViewport(0, 0, width, height);
    mApp->setCamera(SE_Application::MAIN_CAMERA, camera);
    mApp->setCurrentCamera(SE_Application::MAIN_CAMERA);
	SE_InputManager* inputManager = mApp->getInputManager();
    inputManager->removeMotionEventObserver(NULL);
	inputManager->addMotionEventOberver(mApp->getCurrentCamera());
	*/
}
////////////////
SE_2DRunAllAnimation::SE_2DRunAllAnimation(SE_Application* app) : SE_Command(app)
{}
SE_2DRunAllAnimation::~SE_2DRunAllAnimation()
{}
class SE_RunAllAnimationTravel : public SE_SpatialTravel
{
public:
    int visit(SE_Spatial* spatial)
    {
		/*
		SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
		SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		SE_ElementID elementID = spatial->getElementID();
		SE_AnimationID animID = spatial->getAnimationID();
		animManager->removeAnimation(animID);
		SE_Element* element = elementManager->findByID(elementID);
		if(element)
		{
			SE_Animation* anim = element->getAnimation();
			SE_Animation* cloneAnim = NULL;
			if(anim)
			{
				cloneAnim = anim->clone();
				animID = animManager->addAnimation(cloneAnim);
				spatial->setAnimationID(animID);
				cloneAnim->setRunMode(SE_Animation::REPEAT);
				cloneAnim->run();
			}
		}
		*/
		return 0;
    }
    int visit(SE_SimObject* simObject)
	{
        return 1;
	}
};
void SE_2DRunAllAnimation::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	/*
	SE_SceneManager* sceneManager = mApp->getSceneManager();
	SE_Spatial* root = sceneManager->getRoot();
	SE_RunAllAnimationTravel rat;
	root->travel(&rat, true);
	*/
}
