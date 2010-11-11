#include "SE_Sequence.h"
void SE_Sequence::setFrame(unsigned int key, const SE_Sequence::_Frame& frame)
{
    SE_KeyFrame<_Frame>* kf = new SE_KeyFrame<_Frame>;
    kf->key = key;
    kf->data = frame;
    mSequenceFrame.setKeyFrame(kf);
}
SE_Sequence::_Frame SE_Sequence::getFrame(unsigned int key)
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
SE_MountPoint SE_Sequence::getMountPoint(const SE_MountPointID& id)
{
    return mMountPointSet.getMountPoint(id);
}
