#include "SE_Sequence.h"
#include "SE_TimeKey.h"
#include "SE_ImageElement.h"
void SE_Sequence::setFrame(const SE_TimeKey& key, const SE_Sequence::_Frame& frame)
{
    SE_KeyFrame<_Frame>* kf = new SE_KeyFrame<_Frame>;
    kf->key = key;
    kf->data = frame;
    mSequenceFrame.setKeyFrame(kf);
}
SE_Sequence::_Frame SE_Sequence::getFrame(const SE_TimeKey& key) const
{
    SE_KeyFrame<_Frame>* kf = mSequenceFrame.getKeyFrame(key);
    if(!kf)
        return _Frame();
    return kf->data;
}
void SE_Sequence::addMountPoint(const SE_MountPoint& mp)
{
    mMountPointSet.addMountPoint(mp);
}
SE_MountPoint SE_Sequence::getMountPoint(const SE_MountPointID& id) const
{
    return mMountPointSet.getMountPoint(id);
}
std::vector<SE_MountPoint> SE_Sequence::getMountPoint() const
{
	return mMountPointSet.getMountPoint();
}
std::list<SE_Element*> SE_Sequence::createElement(const SE_TimeKey& timeKey)
{
    std::list<SE_Element*> ret;
    std::vector<SE_TimeKey> keys = getKeys();
	SE_ASSERT(keys.size() > 0);
    for(int i = 0 ; i < keys.size() ; i++)
    {
        if(keys[i] > timeKey)
        {
            if(i > 0)
            {
                _Frame f = getFrame(keys[i - 1]);
                SE_ImageElement* e = new SE_ImageElement(f.imageref);
                e->setMountPointRef(f.mpref);
                e->setTimeKey(keys[i]);
                ret.push_back(e);
            }
        }
    }
	if(timeKey >= keys[keys.size() - 1])
	{
        _Frame f = getFrame(keys[keys.size() - 1]);
        SE_ImageElement* e = new SE_ImageElement(f.imageref);
        e->setMountPointRef(f.mpref);
        e->setTimeKey(keys[keys.size() - 1]);
        ret.push_back(e);
	}
    return ret; 
}
