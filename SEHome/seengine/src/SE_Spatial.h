#ifndef SE_SPATIAL_H
#define SE_SPATIAL_H
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_Layer.h"
#include "SE_Object.h"
#include "SE_RenderState.h"
#include "SE_TreeStruct.h"
class SE_BoundingVolume;
class SE_Spatial;
class SE_SimObject;
class SE_BufferInput;
class SE_BufferOutput;
class SE_Camera;
class SE_RenderManager;
class SE_Spatial;
class SE_SpatialTravel
{
public:
    virtual ~SE_SpatialTravel() {}
    virtual int visit(SE_Spatial* spatial) = 0;
    virtual int visit(SE_SimObject* simObject) = 0;
};
class SE_Spatial : public SE_Object, public SE_TreeStruct<SE_Spatial>
{
    DECLARE_OBJECT(SE_Spatial);
public:
    enum {VISIBILITY_MASK = 0x01, MOVABILITY_MASK = 0x02, COLLISION_MASK = 0x04, SELECTED_MASK = 0x08};
    enum {VISIBLE = 0x1, MOVABLE = 0x2, COLLISIONABLE = 0x4, SELECTED = 0x8};
	enum SPATIAL_TYPE {NONE, NODE, GEOMETRY};
	enum RENDER_STATE_TYPE {DEPTHTESTSTATE, BLENDSTATE, RENDERSTATE_NUM};
	enum RENDER_STATE_SOURCE {INHERIT_PARENT, SELF_OWN};
    SE_Spatial();
    virtual ~SE_Spatial();
    const SE_Matrix4f& getWorldTransform();
    SE_Spatial* getParent();
    //SE_Spatial* setParent(SE_Spatial* parent);
    //SE_Vector3f getWorldTranslate();
    //SE_Matrix3f getWorldRotateMatrix();
    //SE_Quat getWorldRotate();
    //SE_Vector3f getWorldScale();
    SE_Vector3f getLocalTranslate();
    SE_Matrix3f getLocalRotateMatrix();
    SE_Quat getLocalRotate();
    SE_Vector3f getLocalScale();
	/*
    SE_SpatialID getSpatialID()
    {
        return mSpatialID;
    }
    void setSpatialID(const SE_SpatialID& spatialID)
    {
        mSpatialID = spatialID;
    }
	*/
    void setLocalTranslate(const SE_Vector3f& translate);
    void setLocalRotate(const SE_Quat& rotate);
    void setLocalRotate(const SE_Matrix3f& rotate);
    void setLocalScale(const SE_Vector3f& scale);
    SE_Vector3f localToWorld(const SE_Vector3f& v);
    SE_Vector3f worldToLocal(const SE_Vector3f& v);
    void setPrevMatrix(const SE_Matrix4f& m)
    {
        mPrevMatrix = m;
    }
    SE_Matrix4f getPrevMatrix()
    {
        return mPrevMatrix;
    }
    void setPostMatrix(const SE_Matrix4f& m)
    {
        mPostMatrix = m;
    } 
    SE_Matrix4f getPostMatrix()
    {
        return mPostMatrix;
    }
	SE_BoundingVolume* getWorldBoundingVolume()
	{
		return mWorldBoundingVolume;
	}
	void setWorldBoundingVolume(SE_BoundingVolume* bv)
	{
		mWorldBoundingVolume = bv;
	}
    int getBVType()
    {
        return mBVType;
    }
    void setBVType(int bvType)
    {
        mBVType = bvType;
    }
    bool isVisible()
    {
        return (mState & VISIBILITY_MASK) == VISIBLE;
    }
    void setVisible(bool v)
    {
        if(v)
        {
            mState |= VISIBLE;
        }
        else
        {
            mState &= (~VISIBLE);
        }
    }
    bool canMove()
    {
        return (mState & MOVABILITY_MASK) == MOVABLE;
    }
    void setMovable(bool m)
    {
        if(m)
        {
            mState |= MOVABLE;
        }
        else
        {
            mState &= (~MOVABLE);
        }
    }
    bool canDoCollision()
    {
        return (mState & COLLISION_MASK) == COLLISIONABLE;
    }
    void setCollisionable(bool c)
    {
        if(c)
        {
            mState |= COLLISIONABLE;
        }
        else
        {
            mState &= (~COLLISIONABLE);
        }
    }
	void setSelected(bool selected)
	{
		if(selected)
		{
            mState |= SELECTED;
		}
		else
		{
			mState &= (~SELECTED);
		}
	}
	bool isSelected()
	{
		return (mState & SELECTED_MASK) == SELECTED; 
	}
	void setRenderTarget(const SE_RenderTargetID& renderTargetID)
	{
		mRenderTargetID = renderTargetID;
	}
	SE_RenderTargetID getRenderTarget()
	{
		return mRenderTargetID;
	}
	void setRenderQueue(int rq)
	{
		mRQ = rq;
	}
	int getRenderQueue()
	{
		return mRQ;
	}
    void setLocalLayer(const SE_Layer& layer)
    {
        mLocalLayer = layer;
    }
    SE_Layer getLocalLayer()
    {
        return mLocalLayer;
    }
    SE_Layer getWorldLayer()
    {
        return mWorldLayer;
    }
	void setAnimationID(const SE_AnimationID& animID)
	{
		mAnimationID = animID;
	}
	SE_AnimationID getAnimationID()
	{
		return mAnimationID;
	}
	void setElementID(const SE_ElementID& eid)
	{
		mElementID = eid;
	}
	SE_ElementID getElementID()
	{
		return mElementID;
	}
	void setOwnRenderTargetCamera(bool own)
	{
		mOwnRenderTargetCamera = own;
	}
	bool isOwnRenderTargetCamera()
	{
		return mOwnRenderTargetCamera;
	}
	void setSceneRenderSeq(const SE_SceneRenderSeq& seq)
	{
		mSceneRenderSeq = seq;
	}
	SE_SceneRenderSeq getSceneRenderSeq() const
	{
		return mSceneRenderSeq;
	}
	void setRenderState(RENDER_STATE_TYPE type, SE_RenderState* rs, SE_OWN_TYPE own);
	//dont use getRenderState to get some spatial's renderstate and then 
	// set it to the other spatial;
	SE_RenderState* getRenderState(RENDER_STATE_TYPE type);
	void setRenderStateSource(RENDER_STATE_TYPE type, RENDER_STATE_SOURCE rsSource);
	void setNeedUpdateTransform(bool b)
	{
		mNeedUpdateTransform = b;
	}
public:
    virtual void attachSimObject(SE_SimObject* go);
    virtual void detachSimObject(SE_SimObject* go);
    virtual int travel(SE_SpatialTravel* spatialTravel, bool travelAways);
    virtual void updateWorldTransform();
    virtual void updateBoundingVolume();
    virtual void updateWorldLayer();
	virtual void updateRenderState();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void renderScene(SE_Camera* camera, SE_RenderManager* renderManager);
	virtual int getSpatialType();
protected:
    //void updateWorldTranslate();
    //void updateWorldRotate();
    //void updateWorldScale();
protected:
    SE_BoundingVolume* mWorldBoundingVolume;
private:
	struct _RenderStateData
	{
		SE_OWN_TYPE own;
		SE_RenderState* renderState;
		_RenderStateData()
		{
			own = OWN;
			renderState= NULL;
		}
		~_RenderStateData()
		{
			if(renderState && own == OWN)
				delete renderState;
		}
	};
	struct _RenderStateProperty
	{
        SE_Wrapper<_RenderStateData>* renderData;
		RENDER_STATE_SOURCE renderSource;
		_RenderStateProperty()
		{
			renderData = NULL;
			renderSource = INHERIT_PARENT;
		}
	};
    SE_Matrix4f mWorldTransform;

    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;

    SE_Vector3f mWorldTranslate;
    SE_Vector3f mWorldScale;
    SE_Quat mWorldRotate;

    int mState;
    int mBVType;
    SE_Layer mLocalLayer;
    SE_Layer mWorldLayer;
	_RenderStateProperty mRenderState[RENDERSTATE_NUM];
    SE_Matrix4f mPrevMatrix;
    SE_Matrix4f mPostMatrix;
	SE_AnimationID mAnimationID;
	SE_ElementID mElementID;
	SE_RenderTargetID mRenderTargetID;
	int mRQ;
	bool mNeedUpdateTransform;
	bool mOwnRenderTargetCamera;
	SE_SceneRenderSeq mSceneRenderSeq;
};
#endif
