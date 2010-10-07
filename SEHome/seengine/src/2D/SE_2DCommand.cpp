#include "SE_2DCommand.h"
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
#include <math.h>
#include <wchar.h>
#include <string.h>
SE_Init2D::SE_Init2D(SE_Application* app) : SE_Command(app)
{
    width = 480;
    height = 800;
}
SE_Init2D::~SE_Init2D()
{}

void SE_Init2D::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->setDataPath(dataPath.c_str());
    SE_ElementManager* elementManager = mApp->getElementManager();
    elementManager->load(NULL, fileName.c_str());
    SE_SceneManager* sceneManager = mApp->getSceneManager();
    SE_Spatial* root = elementManager->createSpatial();
    SE_DepthTestState* rs = new SE_DepthTestState();
	rs->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
    SE_BlendState* blendRs = new SE_BlendState;
    blendRs->setBlendProperty(SE_BlendState::BLEND_ENABLE);
    blendRs->setBlendSrcFunc(SE_BlendState::SRC_ALPHA);
    blendRs->setBlendDstFunc(SE_BlendState::ONE_MINUS_SRC_ALPHA);
	root->setRenderState(SE_Spatial::DEPTHTESTSTATE, rs, OWN);
    root->setRenderState(SE_Spatial::BLENDSTATE, blendRs, OWN);
	root->updateWorldTransform();
    root->updateWorldLayer();
	root->updateRenderState();
    sceneManager->addSpatial(NULL, root);
    SE_Vector3f location(0, 0, 1000);
    float ratio = height /width;
    float angle = 2 * SE_RadianToAngle(atanf(width / 2000));
    SE_Camera* camera = new SE_MotionEventCamera;
	camera->create(location, SE_Vector3f(0, 0, 1), SE_Vector3f(0, 1, 0), angle, ratio, 1, 2000);//(location, SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), angle * 2, ratio, 1, 20);
	camera->setViewport(0, 0, width, height);
    mApp->setCamera(SE_Application::MAIN_CAMERA, camera);
    mApp->setCurrentCamera(SE_Application::MAIN_CAMERA);
	SE_InputManager* inputManager = mApp->getInputManager();
    inputManager->removeMotionEventObserver(NULL);
	inputManager->addMotionEventOberver(mApp->getCurrentCamera());
}
//////////////
SE_2DAnimation::SE_2DAnimation(SE_Application* app, SE_TimeMS duration, TIME_TYPE type) : SE_Command(app, duration, type)
{
	n = 0;
}
SE_2DAnimation::~SE_2DAnimation()
{}
static int eyeCoord[] = {0, 64, 224};
void SE_2DAnimation::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	SE_ImageDataID eyeData("eye");
	SE_ImageData* imageData = SE_Application::getInstance()->getResourceManager()->getImageData(eyeData);
	SE_RectPrimitive* leftEyePrimitive = (SE_RectPrimitive*)SE_Application::getInstance()->getResourceManager()->getPrimitive(leftEyeID);
	SE_RectPrimitive* rightEyePrimitive = (SE_RectPrimitive*)SE_Application::getInstance()->getResourceManager()->getPrimitive(rightEyeID);
    if(n >= 0 && n < 3)
	{
		float x = eyeCoord[n];
		leftEyePrimitive->setImageData(imageData, SE_Texture::TEXTURE0, NOT_OWN, SE_ImageDataPortion(x, 0, 32, 32));
		SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
		sceneManager->removeSpatial(leftEyeSpatialID);
		SE_Spatial* rootNode = sceneManager->getRoot();
		SE_Mesh** leftEyeMesh;
		int leftEyeMeshNum;
		leftEyePrimitive->createMesh(leftEyeMesh, leftEyeMeshNum);
		for(int i = 0 ; i < leftEyeMeshNum ; i++)
		{
			SE_SpatialID geomID = leftEyeSpatialID;//mApp->createCommonID();
			SE_Geometry* geom = new SE_Geometry(geomID, rootNode);
			geom->setLocalLayer(SE_Layer(SE_Layer::LAYER2));
			rootNode->addChild(geom);
			sceneManager->addSpatial(rootNode, geom);
			SE_MeshSimObject* simObj = new SE_MeshSimObject(leftEyeMesh[i], OWN);
			simObj->setName("body");
			geom->attachSimObject(simObj);
			geom->setLocalTranslate(SE_Vector3f(-29, 314, 0));
			geom->setLocalScale(SE_Vector3f(32 / (float)2, 32 / (float)2, 1));
		}
		rootNode->updateWorldLayer();
		rootNode->updateWorldTransform();
		n++;
	}
	else
	{
		n = 0;
	}
	if(duration > 0)
	{
	    SE_2DAnimation* c = new SE_2DAnimation(SE_Application::getInstance(), 60, SE_Command::SIMULATE);
	    c->n = n;
	    c->leftEyeID = leftEyeID;
	    c->rightEyeID = rightEyeID;
	    c->duration = duration - simulateDelta;
		c->leftEyeSpatialID = leftEyeSpatialID;
		mApp->postCommand(c);
	}

}
