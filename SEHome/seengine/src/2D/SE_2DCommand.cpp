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
#include <math.h>
#include <wchar.h>
#include <string.h>
SE_Init2D::SE_Init2D(SE_Application* app) : SE_Command(app)
{
    width = 480;
    height = 800;
	data = NULL;
}
SE_Init2D::~SE_Init2D()
{}
static void readShader()
{
	std::string vertexShaderPath = "C:\\model\\newhome3\\main_vertex_shader.glsl";
	std::string fragmentShaderPath = "C:\\model\\newhome3\\main_fragment_shader.glsl";
	char* vertexShader;
	char* fragmentShader;
	int vertexShaderLen =0;
	int fragmentShaderLen = 0;
	SE_IO::readFileAll(vertexShaderPath.c_str(), vertexShader, vertexShaderLen);
	SE_IO::readFileAll(fragmentShaderPath.c_str(), fragmentShader, fragmentShaderLen);
	char* vs = new char[vertexShaderLen + 1];
	char* fs = new char[fragmentShaderLen + 1];
	memset(vs, 0, vertexShaderLen + 1);
	memset(fs, 0, fragmentShaderLen + 1);
	memcpy(vs, vertexShader, vertexShaderLen);
	memcpy(fs, fragmentShader, fragmentShaderLen);
	SE_ProgramDataID id("main_vertex_shader");
	SE_Application::getInstance()->getResourceManager()->setShaderProgram(id, vs, fs);
	delete[] vertexShader;
	delete[] fragmentShader;
}
void SE_Init2D::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
#if defined(WIN32)
	readShader();
    PCWSTR bodyFilePath = L"C:\\model\\test\\²âÊÔ12.png";
    PCWSTR eyeSequenceFilePath = L"C:\\model\\test\\ÑÛ¾¦_²âÊÔ.png";
    SE_ImageData* imgData1 = SE_ImageCodec::load(bodyFilePath);
    SE_ImageData* imgData2 = SE_ImageCodec::load(eyeSequenceFilePath);
    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    SE_ImageDataID bodyData("body");
    SE_ImageDataID eyeData("eye");
    resourceManager->setImageData(bodyData, imgData1);
    resourceManager->setImageData(eyeData, imgData2);
    float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), e);
    SE_RectPrimitive* rectPrimitive = NULL;
    SE_PrimitiveID bodyPrimitiveID;
    SE_RectPrimitive::create(rect3D, rectPrimitive, bodyPrimitiveID);
    rectPrimitive->setImageData(imgData1, SE_Texture::TEXTURE0, NOT_OWN);
    SE_Mesh** bodyMesh;
    int bodyMeshNum;
    rectPrimitive->createMesh(bodyMesh, bodyMeshNum);
    SE_SpatialID rootID = mApp->createCommonID();
    SE_CommonNode* rootNode = new SE_CommonNode(rootID, NULL);
    SE_SceneManager* sceneManager = mApp->getSceneManager();
    sceneManager->addSpatial(NULL, rootNode);
    for(int i = 0 ; i < bodyMeshNum ; i++)
    {
        SE_SpatialID geomID = mApp->createCommonID();
        SE_Geometry* geo = new SE_Geometry(geomID, rootNode);
		geo->setLocalLayer(SE_Layer(SE_Layer::LAYER1));
        rootNode->addChild(geo);
        sceneManager->addSpatial(rootNode, geo);
        SE_MeshSimObject* simObj = new SE_MeshSimObject(bodyMesh[i], OWN);
		simObj->setName("body");
        geo->attachSimObject(simObj);
        float sw = width / ((float)e[0] * 2);
        float sh = height / ((float)e[1] * 2);
        geo->setLocalScale(SE_Vector3f(sw, sh, 1));
    }
    SE_RectPrimitive* leftEyePrimitive;
    SE_PrimitiveID leftEyePrimitiveID;
    SE_RectPrimitive* rightEyePrimitive;
    SE_PrimitiveID rightEyePrimitiveID;
    SE_RectPrimitive::create(rect3D, leftEyePrimitive, leftEyePrimitiveID);
    SE_RectPrimitive::create(rect3D, rightEyePrimitive, rightEyePrimitiveID);
	data->leftEyeID = leftEyePrimitiveID;
	data->rightEyeID = rightEyePrimitiveID;
    leftEyePrimitive->setImageData(imgData2, SE_Texture::TEXTURE0,NOT_OWN, SE_ImageDataPortion(0, 0, 32, 32));
    rightEyePrimitive->setImageData(imgData2, SE_Texture::TEXTURE0, NOT_OWN, SE_ImageDataPortion(32, 0, 32, 32));
    SE_Mesh** leftEyeMesh;
    int leftEyeMeshNum;
    leftEyePrimitive->createMesh(leftEyeMesh, leftEyeMeshNum);
    for(int i = 0 ; i < leftEyeMeshNum ; i++)
    {
        SE_SpatialID geomID = mApp->createCommonID();
		data->leftSpatialID = geomID;
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
    SE_Mesh** rightEyeMesh;
    int rightEyeMeshNum;
	rightEyePrimitive->createMesh(rightEyeMesh, rightEyeMeshNum);
    for(int i = 0 ; i < rightEyeMeshNum; i++)
    {
        SE_SpatialID geomID = mApp->createCommonID();
        SE_Geometry* geom = new SE_Geometry(geomID, rootNode);
		geom->setLocalLayer(SE_Layer(SE_Layer::LAYER2));
        rootNode->addChild(geom);
        sceneManager->addSpatial(rootNode, geom);
        SE_MeshSimObject* simObj = new SE_MeshSimObject(rightEyeMesh[i], OWN);
		simObj->setName("body");
        geom->attachSimObject(simObj);
        geom->setLocalTranslate(SE_Vector3f(12, 314, 0));
        geom->setLocalScale(SE_Vector3f(32 / (float)2, 32 / (float)2, 1));
    }
	rootNode->updateWorldTransform();
	rootNode->updateWorldLayer();
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
#endif
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
