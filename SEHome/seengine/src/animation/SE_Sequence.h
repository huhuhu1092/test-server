#ifndef SE_SEQUENCE_H
#define SE_SEQUENCE_H
#include "SE_TableManager.h"
#include "SE_ID.h"
#include "SE_MountPoint.h"
#include "SE_KeyFrame.h"
class SE_Sequence
{
public:
    struct _Frame
    {
        SE_StringID imageref;
        SE_MountPointID mpref;
    };
    void setFrame(unsigned int key, const _Frame& f);
	_Frame getFrame(unsigned int);
    SE_MountPoint getMountPoint(const SE_MountPointID& id);
	void addMountPoint(const SE_MountPoint& mp);
	std::vector<SE_MountPoint> getMountPoint();
	std::vector<unsigned int> getKeys()
	{
		return mSequenceFrame.getKeys();
	}
    void setPivotX(int pivotx)
    {
        mPivotX = pivotx;
    }
    int getPivotX()
    {
        return mPivotX;
    }
    void setPivotY(int pivoty)
    {
        mPivotY = pivoty;
    }
    int getPivotY()
    {
        return mPivotY;
    }
private:
    int mPivotX;
    int mPivotY;
    SE_KeyFrameSequence<_Frame> mSequenceFrame;
    SE_MountPointSet mMountPointSet;
};
typedef SE_Table<SE_StringID, SE_Sequence*> SE_SequenceSet;
typedef SE_Table<SE_StringID, SE_SequenceSet*> SE_SequenceTable;
#endif
