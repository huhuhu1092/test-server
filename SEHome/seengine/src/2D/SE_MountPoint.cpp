#include "SE_MountPoint.h"
#include "SE_Buffer.h"
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
void SE_MountPoint::read(SE_BufferInput& input)
{
	mID.read(input);
	mX = input.readFloat();
	mY = input.readFloat();
}
void SE_MountPoint::write(SE_BufferOutput& output)
{
	mID.write(output);
	output.writeFloat(mX);
	output.writeFloat(mY);
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

SE_MountPoint SE_MountPointSet::getMountPoint(const SE_MountPointID& mountPointID) const
{
	_MountPointMap::const_iterator it = mMountPointMap.find(mountPointID);
	if(it != mMountPointMap.end())
		return it->second;
	else
		return SE_MountPoint();
}
std::vector<SE_MountPoint> SE_MountPointSet::getMountPoint() const
{
    _MountPointMap::const_iterator it;
    std::vector<SE_MountPoint> ret(mMountPointMap.size());
    int i = 0;
    for(it = mMountPointMap.begin(); it != mMountPointMap.end(); it++)
    {
		ret[i++] = it->second;
    } 
    return ret;
}
void SE_MountPointSet::read(SE_BufferInput& input)
{
	clearMountPoint();
	int count = input.readInt();
	for(int i = 0 ; i < count  ; i++)
	{
		SE_MountPoint p;
		p.read(input);
		mMountPointMap[p.getID()] = p;
	}
}
void SE_MountPointSet::write(SE_BufferOutput& output)
{
    std::vector<SE_MountPoint> mountPoints = getMountPoint();
	output.writeInt(mountPoints.size());
	for(int i = 0 ; i < mountPoints.size() ; i++)
	{
		mountPoints[i].write(output);
	}
}