#ifndef SE_CAMERAMANAGER_H
#define SE_CAMERAMANAGER_H
#include "SE_ObjectManager.h"
#include "SE_ID.h"
class SE_Camera;
class SE_CameraManager
{
public:
	SE_CameraManager();
	~SE_CameraManager();
	void setCamera(const SE_CameraID& id, SE_Camera* c);
	SE_Camera* getCamera(const SE_CameraID& id);
	void removeCamera(const SE_CameraID& id);
private:
	SE_CameraManager(const SE_CameraManager&);
	SE_CameraManager& operator=(const SE_CameraManager&);
private:
	SE_ObjectManager<SE_CameraID, SE_Camera*> mCameraSet;
};
#endif