#ifndef SE_BONE_H
#define SE_BONE_H
#include "SE_Common.h"
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Matrix.h"
#include <string>
#include <list>
#include <vector>
struct SE_JointPose
{
	SE_Quat rotation;
	SE_Vector3f translate;
	SE_Vector3f scale;
};
struct SE_JointSample
{
	std::vector<SE_JointPose> jointposes;
	std::vector<float> times;
};
struct SE_Joint
{
	SE_Matrix4f bindpose;
	std::vector<SE_JointSample> samples;
	int parentIndex;
	SE_Joint()
	{
		parentIndex = -1;
	}
};
class SE_Skeleton
{
public:
	SE_Skeleton();
	~SE_Skeleton();
	void setJointNum(int num);
	int getJointNum() const
	{
		return mJoints.size();
	}
    void setJoint(int index, const SE_Joint& joint)
	{
		SE_ASSERT(index >= 0 && index < mJoints.size());
		mJoints[index] = joint;
	}
	SE_Joint getJoint(int index)
	{
		SE_ASSERT(index >= 0 && index < mJoints.size());
		return mJoints[index];
	}
private:
	std::vector<SE_Joint> mJoints;
};
class SE_Bone
{
public:
    SE_Bone();
    ~SE_Bone();
    void setParent(SE_Bone* parent)
    {
        mParent = parent;
    }
    SE_Bone* getParent()
    {
        return mParent;
    }
    void setName(const char* name)
    {
        mName = name;
    }
    std::string getName()
    {
        return mName;
    }
    void setMatrixArray(float* data, int num);
    SE_Matrix4f getMatrix(int index)
    {
		int num = mMatrixArray.size();
		if(index < 0 || index >= num)
		{
			return mMatrixBase;
		}
        return mMatrixArray[index];
    }
    int getMatrixNum()
    {
        return mMatrixArray.size();
    }
    void setBaseMatrix(const SE_Matrix4f& m)
    {
        mMatrixBase = m;
    }
    SE_Matrix4f getBaseMatrix()
    {
        return mMatrixBase;
    }
    void addChild(SE_Bone* b)
    {
        mChildrenList.push_back(b);
    }
private:
    typedef std::list<SE_Bone*> _ChildrenList;
    std::string mName;
    _ChildrenList mChildrenList;
    SE_Bone* mParent;
    std::vector<SE_Matrix4f> mMatrixArray;
    SE_Matrix4f mMatrixBase;
};
#endif
