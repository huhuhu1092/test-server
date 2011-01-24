#ifndef SE_CAMERAMANAGER_H
#define SE_CAMERAMANAGER_H
//#include "SE_ObjectManager.h"
#include "SE_ID.h"
#include "SE_TreeStructManager.h"
class SE_Camera;
class SE_CameraManager
{
public:
	SE_CameraManager();
	~SE_CameraManager();
    SE_CameraID addCamera(SE_Camera* c);
	SE_Camera* getCamera(const SE_CameraID& id);
	void removeCamera(const SE_CameraID& id);
private:
	SE_CameraManager(const SE_CameraManager&);
	SE_CameraManager& operator=(const SE_CameraManager&);
private:
    SE_TreeStructManager<SE_Camera> mCameraSet;
//	SE_ObjectManager<SE_CameraID, SE_Camera*> mCameraSet;
};
#endif
