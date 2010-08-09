#ifndef SE_SPATIAL_H
#define SE_SPATIAL_H
#include "SE_Matrix.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_Object.h"
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
    virtual int visit(SE_Spatial* spatial) = 0;
    virtual int visit(SE_SimObject* simObject) = 0;
};
class SE_Spatial : public SE_Object
{
    DECLARE_OBJECT(SE_Spatial);
public:
    enum {VISIBILITY_MASK = 0x01, MOVABILITY_MASK = 0x02, COLLISION_MASK = 0x04};
    enum {VISIBLE = 0x1, MOVABLE = 0x2, COLLISIONABLE = 0x4};
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
    bool setCollisionable(bool c)
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
    virtual void addChild(SE_Spatial* child);
    virtual void removeChild(SE_Spatial* child);
    virtual void attachSimObject(SE_SimObject* go);
    virtual void detachSimObject(SE_SimObject* go);
    virtual int travel(SE_SpatialTravel* spatialTravel, bool travelAways);
    virtual void updateWorldTransform();
    virtual void updateBoundingVolume();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void renderScene(SE_Camera* camera, SE_RenderManager* renderManager);
protected:
    void updateWorldTranslate();
    void updateWorldRotate();
    void updateWorldScale();
protected:
    SE_BoundingVolume* mWorldBoundingVolume;
private:
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
};
#endif
