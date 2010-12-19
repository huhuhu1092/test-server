#include "SE_CameraManager.h"
#include "SE_Camera.h"
SE_CameraManager::SE_CameraManager()
{}
SE_CameraManager::~SE_CameraManager()
{}
void SE_CameraManager::setCamera(const SE_CameraID& id, SE_Camera* c)
{
	mCameraSet.set(id, c);
}
SE_Camera* SE_CameraManager::getCamera(const SE_CameraID& id)
{
	return mCameraSet.get(id);
}
void SE_CameraManager::removeCamera(const SE_CameraID& id)
{
	mCameraSet.remove(id);
}