#include "SE_NetDataCommand.h"
#include "SE_Application.h"
#include "SE_MessageStream.h"
#include "SE_Log.h"
#include "SE_Utils.h"
#include "SE_ImageMap.h"
#include "SE_ImageData.h"
#include "SE_ImageElement.h"
#include "SE_Scene.h"
#include "SE_SceneManager.h"
#include "SE_Camera.h"
#include "SE_ResourceManager.h"
#include "SE_CameraManager.h"
#include <string>
SE_NetDataCommand::SE_NetDataCommand(SE_Application* app, SE_NetMessage* msg) : SE_Command(app)
{
	mMsg = msg;
}
SE_NetDataCommand::~SE_NetDataCommand()
{
	delete mMsg;
}
void SE_NetDataCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	SE_ASSERT(mMsg->len > 3);
	int id = mMsg->data[0];
	unsigned short dataLen = 0;
	memcpy(&dataLen, mMsg->data + 1, 2);
    dataLen = SE_Util::net2HostInt16(dataLen);
	int realDataLen = dataLen - 3;
	std::string str((char*)mMsg->data + 3, realDataLen);
	for(int i = 0 ; i < mMsg->len ; i++)
	{
		LOGI("%d\n", mMsg->data[i]);
	}
	LOGI("### msg len = %d , str = %s ###\n", mMsg->len, str.c_str());
	if(id == 0)
	{
		SE_ImageUnit iu;
		SE_ResourceManager* resourceManager = SE_GET_RESOURCEMANAGER();
		SE_ImageData* imageData = resourceManager->loadImageWithFullPath(str.c_str());
		if(!imageData)
		{
			LOGI("can not load image\n");
			return;
		}
		iu.imageDataID = str.c_str();
		iu.imageRect.x = 0;
		iu.imageRect.y = 0;
		iu.imageRect.width = imageData->getWidth();
		iu.imageRect.height = imageData->getHeight();
		SE_ImageElement* imageElement = new SE_ImageElement("");
		imageElement->setBaseColor(iu);
		SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
		SE_Scene* scene = new SE_2DScene;
		scene->setBackground(SE_Vector4f(1.0f, 1.0f, 1.0f, 1.0f));
		scene->setBound(480, 800);
		scene->setRootElement(imageElement);
		SE_SceneID sceneID = sceneManager->add(scene);
		//create camera
		SE_Camera* camera = SE_Camera::create2DSceneCamera(480, 800);
		SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
		SE_CameraID cameraID = cameraManager->add(camera);
		scene->setCamera(cameraID);
		//end
		sceneManager->show(sceneID);
	}
	
}
