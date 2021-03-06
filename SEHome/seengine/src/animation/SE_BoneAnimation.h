#ifndef SE_BONEANIMATION_H
#define SE_BONEANIMATION_H
#include "SE_Animation.h"
#include "SE_Common.h"
class SE_SkinJointController;
class SE_Mesh;
class SE_SimObject;
class SE_BoneAnimation : public SE_Animation
{
public:
    SE_BoneAnimation();
    ~SE_BoneAnimation();
    void setSkinJointController(SE_SkinJointController* skinJointController)
    {
        mSkinJointController = skinJointController;
    }
    void setSimObject(SE_SimObject* simObject)
    {
        mSimObject = simObject;
    }
public:
    virtual void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
    virtual void onRun();
    virtual SE_Animation* clone();
    virtual void onEnd();
private:
    SE_SkinJointController* mSkinJointController;
    SE_Mesh* mMesh;  
    SE_SimObject* mSimObject;  
	_Vector3f* mVertex;
	int mVertexNum;
};
#endif
