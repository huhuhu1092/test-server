#ifndef SE_SPATIAL_H
#define SE_SPATIAL_H
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_Layer.h"
#include "SE_Object.h"
#include "SE_RenderState.h"
class SE_BoundingVolume;
class SE_Spatial;
class SE_SimObject;
class SE_BufferInput;
class SE_BufferOutput;
class SE_Camera;
class SE_RenderManager;
class SE_SpatialTravel
{
public:
    virtual ~SE_SpatialTravel() {}
    virtual int visit(SE_Spatial* spatial) = 0;
    virtual int visit(SE_SimObject* simObject) = 0;
};
class SE_Spatial : public SE_Object
{
    DECLARE_OBJECT(SE_Spatial);
public:
    enum {VISIBILITY_MASK = 0x01, MOVABILITY_MASK = 0x02, COLLISION_MASK = 0x04, SELECTED_MASK = 0x08};
    enum {VISIBLE = 0x1, MOVABLE = 0x2, COLLISIONABLE = 0x4, SELECTED = 0x8};
	enum SPATIAL_TYPE {NONE, NODE, GEOMETRY};
	enum RENDER_STATE_TYPE {DEPTHTESTSTATE, BLENDSTATE, RENDERSTATE_NUM};
	enum RENDER_STATE_SOURCE {INHERIT_PARENT, SELF_OWN};
    SE_Spatial(SE_Spatial* parent = NULL);
    SE_Spatial(SE_SpatialID spatialID, SE_Spatial* parent = NULL);
    virtual ~SE_Spatial();
    const SE_Matrix4f& getWorldTransform();
    SE_Spatial* getParent();
    SE_Spatial* setParent(SE_Spatial* parent);
    SE_Vector3f getWorldTranslate();
    SE_Matrix3f getWorldRotateMatrix();
    SE_Quat getWorldRotate();
    SE_Vector3f getWorldScale();
    SE_Vector3f getLocalTranslate();
    SE_Matrix3f getLocalRotateMatrix();
    SE_Quat getLocalRotate();
    SE_Vector3f getLocalScale();
    SE_SpatialID getSpatialID()
    {
        return mSpatialID;
    }
    void setSpatialID(const SE_SpatialID& spatialID)
    {
        mSpatialID = spatialID;
    }
    void setLocalTranslate(const SE_Vector3f& translate);
    void setLocalRotate(const SE_Quat& rotate);
    void setLocalRotate(const SE_Matrix3f& rotate);
    void setLocalScale(const SE_Vector3f& scale);
    SE_Vector3f localToWorld(const SE_Vector3f& v);
    SE_Vector3f worldToLocal(const SE_Vector3f& v);
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
    bool canVisible()
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
	void setRenderState(RENDER_STATE_TYPE type, SE_RenderState* rs, SE_OWN_TYPE own);
	//dont use getRenderState to get some spatial's renderstate and then 
	// set it to the other spatial;
	SE_RenderState* getRenderState(RENDER_STATE_TYPE type);
	void setRenderStateSource(RENDER_STATE_TYPE type, RENDER_STATE_SOURCE rsSource);
public:
    virtual void addChild(SE_Spatial* child);
    virtual void removeChild(SE_Spatial* child);
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
	virtual SPATIAL_TYPE getSpatialType();
protected:
    void updateWorldTranslate();
    void updateWorldRotate();
    void updateWorldScale();
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

    SE_Spatial* mParent;
    SE_SpatialID mSpatialID;
    int mState;
    int mBVType;
    SE_Layer mLocalLayer;
    SE_Layer mWorldLayer;
	_RenderStateProperty mRenderState[RENDERSTATE_NUM];
};
#endif
