#include "SE_TestCommand.h"
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
#include "SE_TestAnimation.h"
#include <math.h>
#include <wchar.h>
#include <string.h>
///////////////
SE_TestInitCommand::SE_TestInitCommand(SE_Application* app) : SE_Command(app)
{
}
SE_TestInitCommand::~SE_TestInitCommand()
{}

void SE_TestInitCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->setDataPath(dataPath.c_str());
	//resourceManager->loadImageTable("ImageTable1.xml");
    resourceManager->loadImageTable("CharacterColthesImageDefine.xml");
	resourceManager->loadShader("ShaderDefine.xml");
	resourceManager->loadRenderer("RendererDefine.xml");
    SE_ElementManager* elementManager = mApp->getElementManager();
    SE_Element* rootElement = new SE_Element;
	elementManager->setRoot(rootElement);
    SE_SceneManager* sceneManager = mApp->getSceneManager();
	SE_Element* elementRoot = elementManager->getRoot();
	elementRoot->setID("TestElementRoot");
	elementRoot->setLeft(0);
	elementRoot->setTop(0);
	elementRoot->setWidth(480);
	elementRoot->setHeight(800);
	SE_MountPoint mp;
	mp.setX(240);
	mp.setY(400);
	mp.setID("p1");
	elementRoot->addMountPoint(mp);
	SE_ImageElement* imageElement = new SE_ImageElement("");
	imageElement->setID("ImageElement");
	imageElement->setMountPointRef("p1");
	elementRoot->addChild(imageElement);
	SE_TestAnimation* testAnim = new SE_TestAnimation("CharacterColthesImageDefine.xml");
	imageElement->setAnimation(testAnim);
	testAnim->setElementID("ImageElement");
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
	SE_RenderManager* renderManager = SE_Application::getInstance()->getRenderManager();
	renderManager->setBackground(SE_Vector3f(1, 1, 1));
}