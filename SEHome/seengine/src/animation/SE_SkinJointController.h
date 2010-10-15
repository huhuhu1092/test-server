#ifndef SE_SKINJOINTCONTROLLER_H
#define SE_SKINJOINTCONTROLLER_H
#include "SE_Matrix.h"
#include <string>
#include <vector>
class SE_Bone;
class SE_Vector3f;
class SE_BoneWeight
{
public:
    int boneIndex;
    float weight;
    SE_BoneWeight(int index = -1, float w = 0)
    {
        boneIndex = index;
        weight = w;
    }
};
class SE_SkinJointController
{
public:
    // v is the world coordinate
    SE_Vector3f convert(int vertexIndex, int boneMatrixIndex,const SE_Vector3f& v);
	void createBoneBaseMatrixInverse();
	void createBoneToWorldMatrix(int boneMatrixIndex);
public:
    typedef std::vector<SE_BoneWeight> BoneWeightVector;
    std::vector<SE_Bone*> mBoneVector;
    std::string mMeshName;
    std::vector<BoneWeightVector> mVertexBoneWeightInfo;
	std::vector<SE_Matrix4f> mBoneToWorldMatrix;
	std::vector<SE_Matrix4f> mBoneBaseMatrixInverse;
};
#endif
