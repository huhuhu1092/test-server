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


SE_InitAppCommand::SE_InitAppCommand(SE_Application* app) : SE_Command(app)
{}
SE_InitAppCommand::~SE_InitAppCommand()
{}
void SE_InitAppCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    std::string inPath = dataPath + SE_SEP + fileName + ".ASE";
    std::string outPath = dataPath + SE_SEP + fileName;
    ASE_Loader loader(inPath.c_str(), 0, 0);
    loader.Load();
	loader.Write(dataPath.c_str(), outPath.c_str());
    SE_ResourceManager* resourceManager = mApp->getResourceManager();
    resourceManager->setDataPath(dataPath.c_str());
	resourceManager->loadBaseData(fileName.c_str()); 
    SE_SceneManager* sceneManager = mApp->getSceneManager();
	sceneManager->createScene(fileName.c_str());
    SE_CommonNode* rootScene = sceneManager->getRoot();
    rootScene->updateWorldTransform();
	rootScene->updateBoundingVolume();
	mApp->createCamera(SE_Application::MAIN_CAMERA);
	mApp->setCurrentCamera(SE_Application::MAIN_CAMERA);
}
////////////////
SE_UpdateCameraCommand::SE_UpdateCameraCommand(SE_Application* app) : SE_Command(app)
{}
SE_UpdateCameraCommand::~SE_UpdateCameraCommand()
{}
void SE_UpdateCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	SE_Vector3f location(45.4441,	-234.7624,	90.7012);
	SE_Vector3f zAxis(0, -1, 0);
	SE_Vector3f up(0, 0, 1);
	SE_Camera* c = mApp->getCurrentCamera();
	c->create(location, zAxis, up, 90.0f,((float)height)/ width, 1.0f, 1000.0f);
    c->setViewport(0, 0, width, height);
}
