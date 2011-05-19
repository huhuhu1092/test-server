#include "SE_Spatial.h"
#include "SE_Buffer.h"
#include "SE_BoundingVolume.h"
#include "SE_RenderTargetManager.h"
#include "SE_RenderManager.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_RenderTarget.h"
#include "SE_SpatialManager.h"
#include "SE_Element.h"
#include "SE_ElementManager.h"
#include "SE_Log.h"
IMPLEMENT_OBJECT(SE_Spatial)
SE_Spatial::SE_Spatial()
{
    mWorldTransform.identity();
    mPrevMatrix.identity();
    mPostMatrix.identity();
    mLocalTranslate.setZero();
    mLocalScale.set(1.0f, 1.0f, 1.0f);
    mLocalRotate.identity();
    mWorldBoundingVolume = NULL;
    mState = 0;
    mBVType = 0;
    setMovable(true);
    setVisible(true);
    setCollisionable(true);
	mRQ = SE_RQ0;
	mNeedUpdateTransform = true;
	mOwnRenderTargetCamera = false;
}
SE_Spatial::~SE_Spatial() 
{
    if(mWorldBoundingVolume)
        delete mWorldBoundingVolume;
	for(int i = 0 ; i < RENDERSTATE_NUM ; i++)
	{
		_RenderStateProperty* p = &mRenderState[i];
		if(p->renderData)
		{
			p->renderData->dec();
			if(p->renderData->getNum() == 0)
				delete p->renderData;
		}
	}
}
int SE_Spatial::getSpatialType()
{
	return NONE;
}
void SE_Spatial::updateWorldTransform()
{
    //updateWorldScale();
    //updateWorldRotate();
    //updateWorldTranslate();
    SE_Spatial* parent = SE_Application::getInstance()->getSpatialManager()->getParent(getID());
    if(parent && mNeedUpdateTransform)
    {
        SE_Matrix4f parentM = parent->getWorldTransform();
        SE_Matrix4f localM;
		localM.set(mLocalRotate.toMatrix3f(), mLocalScale, mLocalTranslate);
        localM = mPrevMatrix.mul(localM).mul(mPostMatrix);
        mWorldTransform = parentM.mul(localM);
    }
    else
    {
        SE_Matrix4f localM;
		localM.set(mLocalRotate.toMatrix3f(), mLocalScale, mLocalTranslate);
        mWorldTransform = mPrevMatrix.mul(localM).mul(mPostMatrix);
        //mWorldTransform.set(mWorldRotate.toMatrix3f(), mWorldScale, mWorldTranslate);
    }
	if(mOwnRenderTargetCamera)
	{
		SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
		SE_RenderTarget* rt = rm->get(mRenderTargetID);
		if(rt)
		{
			SE_Camera* camera = rt->getCamera();
			SE_RenderTarget::CAMERA_TYPE ct = rt->getCameraType();
			if(ct == SE_RenderTarget::LOCAL_CAMERA)
			{
				camera->transformLocation(mWorldTransform);
			}
		}
	}
}
void SE_Spatial::updateBoundingVolume()
{

}
void SE_Spatial::setRenderStateSource(RENDER_STATE_TYPE type, RENDER_STATE_SOURCE rsSource)
{
    if(type < 0 || type >= RENDERSTATE_NUM)
		return;
	mRenderState[type].renderSource = rsSource;
}
void SE_Spatial::setRenderState(RENDER_STATE_TYPE type, SE_RenderState* rs, SE_OWN_TYPE own)
{
	if(type < 0 || type >= RENDERSTATE_NUM)
		return;
	_RenderStateProperty* p = &mRenderState[type];
	SE_Wrapper<_RenderStateData>* pRenderStateData = p->renderData;
	if(pRenderStateData)
	{
		pRenderStateData->dec();
		if(pRenderStateData->getNum() == 0)
			delete pRenderStateData;
	}
	_RenderStateData* rsd = new _RenderStateData;
	rsd->own = own;
	rsd->renderState = rs;
	pRenderStateData = new SE_Wrapper<_RenderStateData>(rsd, SE_Wrapper<_RenderStateData>::NOT_ARRAY);
	p->renderData = pRenderStateData;
	p->renderSource = SELF_OWN;
}
SE_RenderState* SE_Spatial::getRenderState(RENDER_STATE_TYPE type)
{
    if(type < 0 || type >= RENDERSTATE_NUM)
		return NULL;
	_RenderStateProperty* p = &mRenderState[type];
	SE_Wrapper<_RenderStateData>* renderData = p->renderData;
	if(!renderData)
		return NULL;
	return renderData->getPtr()->renderState;
}
void SE_Spatial::updateRenderState()
{
    SE_Spatial* parent = SE_Application::getInstance()->getSpatialManager()->getParent(getID());
	if(!parent)
		return;
	for(int i = 0 ; i < RENDERSTATE_NUM ; i++)
	{
		_RenderStateProperty* parentRenderProperty = &parent->mRenderState[i];
		SE_Wrapper<_RenderStateData>* parentRenderStateData = parentRenderProperty->renderData;
		_RenderStateProperty* pRenderProperty = &mRenderState[i];
		if(pRenderProperty->renderSource == INHERIT_PARENT && parentRenderStateData)
		{
			if(!pRenderProperty->renderData)
			{
				pRenderProperty->renderData = parentRenderStateData;
				parentRenderStateData->inc();
			}
			else
			{
				pRenderProperty->renderData->dec();
				if(pRenderProperty->renderData->getNum() == 0)
					delete pRenderProperty->renderData;
				pRenderProperty->renderData = parentRenderStateData;
				parentRenderStateData->inc();
			}
		}
	}
}
void SE_Spatial::setRenderTarget(const SE_RenderTargetID& renderTargetID)
{
	mRenderTargetID = renderTargetID;
	std::vector<SE_Spatial*> children = getChildren();
	for(size_t i = 0 ; i < children.size() ; i++)
	{
		SE_Spatial* s = children[i];
		s->setRenderTarget(renderTargetID);
	}
}
void SE_Spatial::setRenderTargetSeq(const SE_RenderTargetSeq& seq)
{
	mRenderTargetSeq = seq;
	std::vector<SE_Spatial*> children = getChildren();
	for(size_t i = 0 ; i < children.size() ; i++)
	{
		SE_Spatial* s = children[i];
		s->setRenderTargetSeq(seq);
	}
}
void SE_Spatial::setSceneRenderSeq(const SE_SceneRenderSeq& seq)
{
	mSceneRenderSeq = seq;
	std::vector<SE_Spatial*> children = getChildren();
	for(size_t i = 0 ; i < children.size() ; i++)
	{
		SE_Spatial* s = children[i];
		s->setSceneRenderSeq(seq);
	}
}

void SE_Spatial::updateSpatialIDToElement()
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Element* element = elementManager->get(mElementID);
	if(element)
	{
		element->setSpatialID(getID());
	}
	std::vector<SE_Spatial*> children = getChildren();
	for(size_t i = 0 ; i < children.size() ; i++)
	{
		SE_Spatial* s = children[i];
		s->updateSpatialIDToElement();
	}
}
const SE_Matrix4f& SE_Spatial::getWorldTransform()
{
    return mWorldTransform;
}

SE_Spatial* SE_Spatial::getParent()
{
    return SE_Application::getInstance()->getSpatialManager()->getParent(getID());
}
/*
SE_Spatial*SE_Spatial::setParent(SE_Spatial* parent)
{
    SE_Spatial* ret = mParent;
    mParent = parent;
    return ret;
}
*/
/*
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
*/
void SE_Spatial::updateWorldLayer()
{
    SE_Spatial* parent = getParent();
    if(parent)
    {
        SE_Layer parentLayer = parent->getWorldLayer();
        mWorldLayer = parentLayer + mLocalLayer;
    }
    else
    {
        mWorldLayer = mLocalLayer;
    }
}
/*
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
*/
SE_Vector3f SE_Spatial::localToWorld(const SE_Vector3f& v)
{
    /*
    SE_Vector3f scaledV = mWorldScale.mul(v);
    SE_Vector3f rotatedV = mWorldRotate.map(scaledV);
    SE_Vector3f translateV = mWorldTranslate.add(rotatedV);
    */
    SE_Vector4f v4(v.x, v.y, v.z, 0);
    v4 = mWorldTransform.map(v4);
    return v4.xyz();
}
SE_Vector3f SE_Spatial::worldToLocal(const SE_Vector3f& v)
{
    /*
    SE_Vector3f translatedV = v.subtract(mWorldTranslate);
    SE_Vector3f rotatedV = mWorldRotate.inverse().map(translatedV);
    SE_Vector3f inverseScale(1 / mWorldScale.x, 1 / mWorldScale.y, 1 / mWorldScale.z);
    SE_Vector3f scaledV = inverseScale.mul(rotatedV);
    */
    SE_Matrix4f inverseTransform = mWorldTransform.inverse();
    SE_Vector4f v4(v.x, v.y, v.z, 0);
    v4 = inverseTransform.map(v4);
    return v4.xyz();
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
/*
void SE_Spatial::addChild(SE_Spatial* child)
{}
void SE_Spatial::removeChild(SE_Spatial* child)
{}
*/
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
    getID().read(input);
    mState = input.readInt();
    mBVType = input.readInt();
    mLocalTranslate = input.readVector3f();
    mLocalScale = input.readVector3f();
    mLocalRotate = input.readQuat();
	mPrevMatrix = input.readMatrix4f();
	mPostMatrix = input.readMatrix4f();
}
void SE_Spatial::write(SE_BufferOutput& output)
{
    getID().write(output);
    output.writeInt(mState);
    output.writeInt(mBVType);
    output.writeVector3f(mLocalTranslate);
    output.writeVector3f(mLocalScale);
    output.writeQuat(mLocalRotate);
	float m[16];
	mPrevMatrix.getSequence(m, 16);
	output.writeFloatArray(m, 16);
	mPostMatrix.getSequence(m, 16);
	output.writeFloatArray(m, 16);
}
void SE_Spatial::renderScene(SE_Camera* camera, SE_RenderManager* renderManager)
{}
