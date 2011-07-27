#ifndef SE_USERCOMMAND_H
#define SE_USERCOMMAND_H
#include "SE_Command.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Primitive.h"

#ifdef ANDROID
#include "SkBitmap.h"
#endif
#include <string>
class SE_Application;
class SE_Spatial;

//The spatial that mSpatialId pointed will rotate mRotateAngle degree,round X,Y or Z axis
class SE_RotateSpatialCommand : public SE_Command
{
public:
    SE_RotateSpatialCommand(SE_Application* app);
    ~SE_RotateSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    std::string mObjectName;

	//rotate angle
	float mRotateAngle;

	//rotate axis
	SE_AXIS_TYPE mAxis;

	bool mAffectGroup;
private:
	//Not used, now
	SE_Vector3f mRotateVector;

};

//The spatial will scale in X,Y or Z axis.
class SE_ScaleSpatialCommand : public SE_Command
{
public:
    SE_ScaleSpatialCommand(SE_Application* app);
    ~SE_ScaleSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    std::string mObjectName;

	float mScaledX;
	float mScaledY;
	float mScaledZ;

	bool mAffectGroup;

};

class SE_TranslateSpatialCommand : public SE_Command
{
public:
    SE_TranslateSpatialCommand(SE_Application* app);
    ~SE_TranslateSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;

    std::string mObjectName;

	float mTranslatedX;
	float mTranslatedY;
	float mTranslatedZ;

	bool mAffectGroup;

};

class SE_ResetSpatialCommand : public SE_Command
{
public:
    SE_ResetSpatialCommand(SE_Application* app);
    ~SE_ResetSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    std::string mObjectName;
    SE_SpatialID mSpatialID;
};


class SE_RemoveSpatialCommand : public SE_Command
{
public:
    SE_RemoveSpatialCommand(SE_Application* app);
     ~SE_RemoveSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    std::string mObjectName;
};

class SE_ReLoadSpatialCommand : public SE_Command
{
public:
    SE_ReLoadSpatialCommand(SE_Application* app);
     ~SE_ReLoadSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    std::string mObjectName;
};


class SE_ReLoadAllSpatialCommand : public SE_Command
{
public:
    SE_ReLoadAllSpatialCommand(SE_Application* app);
     ~SE_ReLoadAllSpatialCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
};

class SE_AddUserObjectCommand : public SE_Command
{
public:
    SE_AddUserObjectCommand(SE_Application* app);
    ~SE_AddUserObjectCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    void setVertexArray(SE_Vector3f* vertexArray, int vertexNum)
    {
	mVertexArray = vertexArray;
	mVertexNum = vertexNum;
    }

    void setVertexIndexArray(SE_Vector3i* vertexIndexArray, int vertexIndexNum)
    {
	mVertexIndexArray = vertexIndexArray;
	mVertexIndexNum = vertexIndexNum;
    }

    void setTextureCoorArray(SE_Vector2f* textureCoorArray, int textureCoorNum)
    {
	mTextureCoorArray = textureCoorArray;
	mTextureCoorNum = textureCoorNum;
    }

    void setTextureCoorIndexArray(SE_Vector3i* textureCoorIndexArray, int textureCoorIndexNum)
    {
	mTextureCoorIndexArray = textureCoorIndexArray;
	mTextureCoorIndexNum = textureCoorIndexNum;
    }

    void setFacetIndexArray(int* facetIndexArray, int facetIndexNum)
    {
	mFacetIndexArray = facetIndexArray;
	mFacetIndexNum = facetIndexNum;
    }

    void setLocalRotate(SE_Quat& localRotate)
    {
	mLocalRotate = localRotate;
    }

    void setLocalTranslate(SE_Vector3f& localTranslate)
    {
	mLocalTranslate = localTranslate;
    }

    void setLocalScale(SE_Vector3f& localScale)
    {
	mLocalScale = localScale;
    }

    void setColor(SE_Vector3f color)
    {
	mColor = color;
    }

public:
    std::string mImagePath;
    std::string mObjectName;
    std::string mType;
    int mLayerIndex;
    bool mNeedBlending;
    bool mNeedDepthTest;
    bool mLastLayerInWorld;
    float mAlpha;
    SE_ProgramDataID mProgramDataID;
    SE_RendererID mRendererID;
    SE_Vector3f mColor;
    SE_Vector3f* mVertexArray;
    SE_Vector3i* mVertexIndexArray;
    int* mFacetIndexArray;
    SE_Vector2f* mTextureCoorArray;
    SE_Vector3i* mTextureCoorIndexArray;
    int mVertexNum;
    int mVertexIndexNum;
    int mFacetIndexNum;
    int mTextureCoorNum;
    int mTextureCoorIndexNum;
    SE_UserObject* mPrimitive;
    SE_Quat mLocalRotate;
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    bool mVisible;
};

class SE_OperateObjectCommand : public SE_Command
{
public:
    SE_OperateObjectCommand(SE_Application* app);
    ~SE_OperateObjectCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    SE_Spatial * mSpatial;
    SE_Vector3f mTranslate;
    SE_Quat mRotate;
    SE_Vector3f mScale; 
};

class SE_AddNewCbfCommand : public SE_Command
{
public:
    SE_AddNewCbfCommand(SE_Application* app);
    ~SE_AddNewCbfCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    std::string mCbfFileNameID;
    std::string dataPath;
};

class SE_OperateCameraCommand : public SE_Command
{
public:
    SE_OperateCameraCommand(SE_Application* app);
    ~SE_OperateCameraCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    SE_Vector3f mLocation;
    bool mTranslate;
    SE_Quat mRotate;
};

class SE_SetObjectAlphaCommand : public SE_Command
{
public:
    SE_SetObjectAlphaCommand(SE_Application* app);
    ~SE_SetObjectAlphaCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    float mAlpha;
};

class SE_SetObjectVisibleCommand : public SE_Command
{
public:
    SE_SetObjectVisibleCommand(SE_Application* app);
    ~SE_SetObjectVisibleCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    std::string mName;
    bool mVisible;
};

class SE_SetObjectRenderStateCommand : public SE_Command
{
public:
    SE_SetObjectRenderStateCommand(SE_Application* app);
    ~SE_SetObjectRenderStateCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    bool mIsBlending;
    bool mDepthTest;
};

class SE_SetObjectLayerCommand : public SE_Command
{
public:
    SE_SetObjectLayerCommand(SE_Application* app);
    ~SE_SetObjectLayerCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;
    int mLayerIndex;
};

class SE_SetObjectLightingPositonCommand : public SE_Command
{
public:
    SE_SetObjectLightingPositonCommand(SE_Application* app);
    ~SE_SetObjectLightingPositonCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    bool mUseVbo;
    SE_SpatialID mSpatialID;
    SE_Vector3f mWorldPositon;
};

class SE_SetObjectNormalMapCommand : public SE_Command
{
public:
    SE_SetObjectNormalMapCommand(SE_Application* app);
    ~SE_SetObjectNormalMapCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    std::string mName;
    SE_Vector3f mWorldPositon;
    bool mUseVbo;
};

class SE_SetObjectDefaultShaderCommand : public SE_Command
{
public:
    SE_SetObjectDefaultShaderCommand(SE_Application* app);
    ~SE_SetObjectDefaultShaderCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
	//Spatialid
    SE_SpatialID mSpatialID;    
};

class SE_UnLoadSceneCommand : public SE_Command
{
public:
    SE_UnLoadSceneCommand(SE_Application* app);
    ~SE_UnLoadSceneCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
};

class SE_DeleteObjectCommand : public SE_Command
{
public:
    SE_DeleteObjectCommand(SE_Application* app);
    ~SE_DeleteObjectCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);

	//The object name will be delete
	std::string mObjectName;
};

class SE_CloneObjectCommand : public SE_Command
{
public:
    SE_CloneObjectCommand(SE_Application* app);
    ~SE_CloneObjectCommand();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);

	//the object need be clone
	std::string mObjectName;
};
#endif
