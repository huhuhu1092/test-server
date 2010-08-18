#include "SE_Spatial.h"
#include "SE_Buffer.h"
#include "SE_BoundingVolume.h"
#include "SE_Log.h"
IMPLEMENT_OBJECT(SE_Spatial)
SE_Spatial::SE_Spatial(SE_Spatial* parent)
{
    mWorldTransform.identity();
    mLocalTranslate.setZero();
    mLocalScale.set(1.0f, 1.0f, 1.0f);
    mLocalRotate.identity();
    mWorldBoundingVolume = NULL;
    mParent = parent;
    mState = 0;
    mBVType = 0;
    setMovable(true);
    setVisible(true);
    setCollisionable(true);
}
SE_Spatial::SE_Spatial(SE_SpatialID spatialID, SE_Spatial* parent)
{
    mWorldTransform.identity();
    mLocalTranslate.setZero();
    mLocalScale.set(1.0f, 1.0f, 1.0f);
    mLocalRotate.identity();
    mWorldBoundingVolume = NULL;
    mParent = parent;
    mSpatialID = spatialID;
    mState = 0;
    mBVType = 0;
    setMovable(true);
    setVisible(true);
    setCollisionable(true);
}
SE_Spatial::~SE_Spatial() 
{
    if(mWorldBoundingVolume)
        delete mWorldBoundingVolume;
}
void SE_Spatial::updateWorldTransform()
{
    updateWorldScale();
    updateWorldRotate();
    updateWorldTranslate();
    mWorldTransform.set(mWorldRotate.toMatrix3f(), mWorldScale, mWorldTranslate);
}
void SE_Spatial::updateBoundingVolume()
{

}

const SE_Matrix4f& SE_Spatial::getWorldTransform()
{
    return mWorldTransform;
}

SE_Spatial* SE_Spatial::getParent()
{
    return mParent;
}
SE_Spatial* SE_Spatial::setParent(SE_Spatial* parent)
{
    SE_Spatial* ret = mParent;
    mParent = parent;
    return ret;
}
SE_Vector3f SE_Spatial::getWorldTranslate()
{
    return mWorldTranslate;
}
SE_Quat SE_Spatial::getWorldRotate()
{
    return mWorldRotate;
}
SE_Matrix3f SE_Spatial::getWorldRotateMatrix()
{
    return mWorldRotate.toMatrix3f();
}
SE_Vector3f SE_Spatial::getWorldScale()
{
    return mWorldScale;
}
void SE_Spatial::updateWorldTranslate()
{
    if(mParent)
    {
        mWorldTranslate = mParent->localToWorld(mLocalTranslate);
    }
    else 
    {
        mWorldTranslate = mLocalTranslate;
    }
}
void SE_Spatial::updateWorldRotate()
{
    if(mParent)
    {
        mWorldRotate = mParent->getWorldRotate().mul(mLocalRotate);
    }
    else
    {
        mWorldRotate = mLocalRotate;
    }
}
void SE_Spatial::updateWorldScale()
{
    if(mParent)
    {
        mWorldScale = mParent->getWorldScale().mul(mLocalScale);
    }
    else
    {
        mWorldScale = mLocalScale;
    }
}
SE_Vector3f SE_Spatial::localToWorld(const SE_Vector3f& v)
{
    SE_Vector3f scaledV = mWorldScale.mul(v);
    SE_Vector3f rotatedV = mWorldRotate.map(scaledV);
    SE_Vector3f translateV = mWorldTranslate.add(rotatedV);
    return translateV;
}
SE_Vector3f SE_Spatial::worldToLocal(const SE_Vector3f& v)
{
    SE_Vector3f translatedV = v.subtract(mWorldTranslate);
    SE_Vector3f rotatedV = mWorldRotate.inverse().map(translatedV);
    SE_Vector3f inverseScale(1 / mWorldScale.x, 1 / mWorldScale.y, 1 / mWorldScale.z);
    SE_Vector3f scaledV = inverseScale.mul(rotatedV);
    return scaledV;
}

SE_Vector3f SE_Spatial::getLocalTranslate()
{
    return mLocalTranslate;
}
SE_Matrix3f SE_Spatial::getLocalRotateMatrix()
{
    return mLocalRotate.toMatrix3f();
}
SE_Quat SE_Spatial::getLocalRotate()
{
    return mLocalRotate;
}
SE_Vector3f SE_Spatial::getLocalScale()
{
    return mLocalScale;
}
void SE_Spatial::setLocalRotate(const SE_Matrix3f& rotate)
{

}
void SE_Spatial::setLocalTranslate(const SE_Vector3f& translate)
{
    mLocalTranslate = translate;
}
void SE_Spatial::setLocalRotate(const SE_Quat& rotate)
{
    mLocalRotate = rotate;
}
void SE_Spatial::setLocalScale(const SE_Vector3f& scale)
{
    mLocalScale = scale;
}
void SE_Spatial::addChild(SE_Spatial* child)
{}
void SE_Spatial::removeChild(SE_Spatial* child)
{}
void SE_Spatial::attachSimObject(SE_SimObject* go)
{}
void SE_Spatial::detachSimObject(SE_SimObject* go)
{}
int SE_Spatial::travel(SE_SpatialTravel* spatialTravel, bool tranvelAlways)
{
	return 0;
}
void SE_Spatial::read(SE_BufferInput& input)
{
    mSpatialID.read(input);
    mState = input.readInt();
    mBVType = input.readInt();
    mLocalTranslate = input.readVector3f();
    mLocalScale = input.readVector3f();
    mLocalRotate = input.readQuat();
}
void SE_Spatial::write(SE_BufferOutput& output)
{
    mSpatialID.write(output);
    output.writeInt(mState);
    output.writeInt(mBVType);
    output.writeVector3f(mLocalTranslate);
    output.writeVector3f(mLocalScale);
    output.writeQuat(mLocalRotate);
}
void SE_Spatial::renderScene(SE_Camera* camera, SE_RenderManager* renderManager)
{}
