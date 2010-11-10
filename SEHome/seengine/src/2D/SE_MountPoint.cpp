#include "SE_MountPoint.h"
SE_MountPoint::SE_MountPoint()
{
    mX = mY = 0;
}
SE_MountPoint::~SE_MountPoint()
{}
SE_MountPoint::SE_MountPoint(float x, float y)
{
    mX = x;
    mY = y;
}
SE_MountPoint::SE_MountPoint(const SE_MountPoint& p)
{
    mX = p.mX;
    mY = p.mY;
}
SE_MountPoint& SE_MountPoint::operator=(const SE_MountPoint& p)
{
    if(this == &p)
        return *this;
    mX = p.mX;
    mY = p.mY;
    return *this;
}
///////////////////////////
void SE_MountPointSet::addMountPoint(const SE_MountPoint& mountPoint)
{
    mMountPointMap[mountPoint.getID()] = mountPoint;
}
void SE_MountPointSet::removeMountPoint(const SE_MountPointID& mountPointID)
{
	_MountPointMap::iterator it = mMountPointMap.find(mountPointID);
    if(it != mMountPointMap.end())
		mMountPointMap.erase(it);
}

void SE_MountPointSet::clearMountPoint()
{
    mMountPointMap.clear();
}

SE_MountPoint SE_MountPointSet::getMountPoint(const SE_MountPointID& mountPointID)
{
	_MountPointMap::iterator it = mMountPointMap.find(mountPointID);
	if(it != mMountPointMap.end())
		return it->second;
	else
		return SE_MountPoint();
}
std::vector<SE_MountPoint> SE_MountPointSet::getMountPoint()
{
    _MountPointMap::iterator it;
    std::vector<SE_MountPoint> ret(mMountPointMap.size());
    int i = 0;
    for(it = mMountPointMap.begin(); it != mMountPointSet.end(); it++)
    {
        ret[i++] = *it;
    } 
    return ret;
}
