#include "SE_CameraManager.h"
#include "SE_Camera.h"
SE_CameraManager::SE_CameraManager(): mCameraSet(100, 512)
{}
SE_CameraManager::~SE_CameraManager()
{}
SE_CameraID SE_CameraManager::addCamera(SE_Camera* c)
{
	return mCameraSet.add(SE_CameraID::NULLID, c);
	//mCameraSet.set(id, c);
}
SE_Camera* SE_CameraManager::getCamera(const SE_CameraID& id)
{
	return mCameraSet.find(id);
}
SE_Camera* SE_CameraManager::removeCamera(const SE_CameraID& id)
{
	return mCameraSet.remove(id);
}
void SE_CameraManager::release(SE_Camera* c, int delay)
{
	mCameraSet.release(c, delay);
}
void SE_CameraManager::release(const SE_CameraID& c , int delay)
{
	mCameraSet.release(c, delay);
}