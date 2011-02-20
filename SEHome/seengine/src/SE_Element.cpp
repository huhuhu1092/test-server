#include "SE_Element.h"
#include "SE_KeyFrameController.h"
#include "SE_SpatialType.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_ParamManager.h"
#include "SE_Geometry3D.h"
#include "SE_Spatial.h"
#include "SE_Buffer.h"
#include "SE_MessageEventCommandDefine.h"
#include "SE_ImageData.h"
#include "SE_Camera.h"
#include "SE_CameraManager.h"
#include "SE_CommonNode.h"
#include "SE_Spatial.h"
#include "SE_Animation.h"
#include "SE_Mesh.h"
#include "SE_MeshSimObject.h"
#include "SE_SimObjectManager.h"
#include "SE_Geometry.h"
#include "SE_MessageEventCommandDefine.h"
#include "SE_AnimationManager.h"
#include <math.h>
//////////////////////////////
class SE_ElementParamUpdateEvent : public SE_ElementEvent
{
public:
    SE_ElementParamUpdateEvent()
    {
        mType = SE_ELEMENTEVENT_UPDATEPARAM;
        mElement = NULL;
    }
    void run();
    bool merge(SE_ElementEvent* mergeEvent);
    void setParamValue(const SE_AddressID& address, const SE_Value& v)
    {
        mParamValueList.add(address, v);
    }
    SE_ParamValueList mParamValueList;
    SE_ElementID mElementID;
    SE_Element* mElement;
};
void SE_ElementParamUpdateEvent::run()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(!mElement)
        mElement = elementManager->get(mElementID);
    if(!mElement)
        return;
    mElement->update(mParamValueList);
}
bool SE_ElementParamUpdateEvent::merge(SE_ElementEvent* mergeEvent)
{
    SE_ElementParamUpdateEvent* pEvent = (SE_ElementParamUpdateEvent*)mergeEvent;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(!mElement)
        mElement = elementManager->get(mElementID);
    SE_Element* mergedElement = elementManager->get(pEvent->mElementID);
    if(!mElement || !mergedElement)
        return true;
    if(mElementID == pEvent->mElementID)
    {
		pEvent->mParamValueList.add(mParamValueList.getParamValue());
        return true;
    }
    else
        return false;
}
//////////////////////////
SE_Element::SE_Element()
{
    mState = NORMAL;
    mType = 0;
    mKeyFrameNum = 0;
    mSeqNum = -1;
    mKeyFrameController = NULL;
    mAnimation = NULL;
    mOwnRenderTargetCamera = false;
    mNeedUpdateTransform = true;
    mRenderQueueSeq =  SE_RQ0;
}

SE_Element::~SE_Element()
{
    if(mKeyFrameController)
        delete mKeyFrameController;
    /*
    std::vector<SE_AddressID> addressV = mURI.getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    } 
    */
    _DeleteURI deleteURI;
    deleteURI.element = this;
    mStateURIManager.traverse(deleteURI);
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
	SE_SimObject* simObject = simObjectManager->remove(mSimObjectID);
	simObjectManager->release(simObject);
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* s = spatialManager->remove(mSpatialID);
    spatialManager->release(s);
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->removePrimitive(mPrimitiveID);
    SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
    animManager->remove(mAnimationID);
}
bool SE_Element::isRoot()
{
    return getParent() == NULL;
}
SE_Element* SE_Element::getParent()
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	return elementManager->getParent(getID());
}
void SE_Element::setKeyFrameController(SE_KeyFrameController* kfc)
{
    if(mKeyFrameController)
        delete mKeyFrameController;
    mKeyFrameController = kfc;
}
void SE_Element::spawn()
{
}
void SE_Element::update(const SE_TimeKey& timeKey)
{

}
void SE_Element::layout()
{}
void SE_Element::updateSpatial()
{
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* spatial = spatialManager->get(getID());
	SE_Element* parent = getParent();
	if(!parent)
		return;
    SE_Spatial* parentSpatial = spatialManager->get(parent->getID());
    if(!spatial && !parentSpatial)
    {
        return ;
    }
    else if(spatial && !parentSpatial)
    {
        SE_ASSERT(0);
    }
    else if(!spatial && parentSpatial)
    {
        SE_Spatial* s = createSpatial();
		spatialManager->add(parentSpatial->getID(), s, true);
    }
    else if(spatial && parentSpatial)
    {
        SE_Spatial* oldSpatial = spatialManager->remove(mSpatialID);
        SE_Spatial* s = createSpatial();
        spatialManager->add(parentSpatial->getID(), s, true);
		spatialManager->release(oldSpatial);
    }
}
SE_Element* SE_Element::clone()
{
	return NULL;
}
SE_Spatial* SE_Element::createSpatial()
{
    return NULL;
}

void SE_Element::read(SE_BufferInput& inputBuffer)
{

}
void SE_Element::write(SE_BufferOutput& outputBuffer)
{
}
void SE_Element::setSceneRenderSeq(const SE_SceneRenderSeq& seq)
{
    mSceneRenderSeq = seq;
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	SE_Spatial* spatial = spatialManager->get(mSpatialID);
    if(spatial)
	{
		spatial->setSceneRenderSeq(seq);
	}
    std::vector<SE_Element*> children = getChildren();
    if(!children.empty())
    {
        for(int i = 0 ; i < children.size() ; i++)
        {
            SE_Element* e = children[i];
            e->setSceneRenderSeq(seq);
        }
    }
}
void SE_Element::setRenderTargetID(const SE_RenderTargetID& renderTarget)
{
    if(!mRenderTarget.isValid())
    {
        mRenderTarget = renderTarget;
    }
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	SE_Spatial* spatial = spatialManager->get(mSpatialID);
    if(spatial)
	{
		spatial->setRenderTarget(mRenderTarget);
	}
    std::vector<SE_Element*> children = getChildren();
    if(!children.empty())
    {
        for(int i = 0 ; i < children.size() ; i++)
        {
            SE_Element* e = children[i];
            e->setRenderTargetID(renderTarget);
        }
    }
}
void SE_Element::travel(SE_ElementTravel* travel)
{
	travel->visit(this);
    std::vector<SE_Element*> children = getChildren();
	if(!children.empty())
    {
        for(int i = 0 ; i < children.size() ; i++)
        {
            SE_Element* e = children[i];
            e->travel(travel);
        }
    }

}
void SE_Element::update(SE_ParamValueList& paramValueList)
{}
void SE_Element::update(const SE_AddressID& address, const SE_Value& value)
{}

void SE_Element::hide()
{}
void SE_Element::clearChildren()
{
	std::vector<SE_Element*> children = getChildren();
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	for(int i = 0 ; i < children.size() ; i++)
	{
		SE_Element* e = children[i];
		elementManager->remove(e->getID());
        elementManager->release(e);
	}
}
void SE_Element::setStateURI(int state, const SE_StringID& uri)
{
	SE_URI tURI(uri.getStr());
	bool isContain = mStateURIManager.isContain(state);
	if(isContain)
	{
		SE_URI oldURI = mStateURIManager.get(state);
		removeObserverFromParamManager(&oldURI);

	}
	mStateURIManager.set(state, tURI);
    addObserverToParamManager(&tURI);
}
void SE_Element::removeObserverFromParamManager(const SE_URI* uri)
{
    std::vector<SE_AddressID> addressV = uri->getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    }   
}
void SE_Element::addObserverToParamManager(const SE_URI* uri)
{
    std::vector<SE_AddressID> addressV = uri->getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    }    
}
SE_URI SE_Element::getURI(int state) const
{
	SE_URI uri = mStateURIManager.get(state);
	return uri;
}
SE_StringID SE_Element::getURL(int state) const
{
	SE_URI uri = mStateURIManager.get(state);
	return uri.getURL();
}
SE_StringID SE_Element::getURI() const
{
	SE_URI uri = getURI(NORMAL);
	return uri.getURI();
}
SE_StringID SE_Element::getURL() const
{
	SE_URI uri = getURI(NORMAL);
	return uri.getURL();
}
void SE_Element::setURI(const SE_StringID& uri)
{
	setStateURI(NORMAL, uri);
	/*
    std::vector<SE_AddressID> addressV = mURI.getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    }    
    mURI.setURI(uri);
    addressV = mURI.getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    } 
	*/
}
void SE_Element::clone(SE_Element *src, SE_Element* dst)
{

	dst->mLocalLayer = src->mLocalLayer;
	dst->mLocalTranslate = src->mLocalTranslate;
	dst->mLocalScale = src->mLocalScale;
	dst->mLocalRotate = src->mLocalRotate;
	dst->mName = src->mName;
	dst->mFullPathName = src->mFullPathName;
    if(src->mKeyFrameController)
        dst->mKeyFrameController = src->mKeyFrameController->clone();
    if(src->mAnimation)
        dst->mAnimation = src->mAnimation->clone();
	dst->mTimeKey = src->mTimeKey;
	dst->mStartKey = src->mStartKey;
	dst->mEndKey = src->mEndKey;
	dst->mKeyFrameNum = src->mKeyFrameNum;
	dst->mSeqNum = src->mSeqNum;
}

///////////////////////////////////////
SE_2DNodeElement::SE_2DNodeElement()
{
    mLeft = mTop = mWidth = mHeight = 0;
    mPivotX = mPivotY = mMountPointX = mMountPointY = 0;
    mDeltaLeft = mDeltaTop = 0;
    mSpatialType = SE_COMMON_NODE_TYPE;
	mRectPatchType = SE_RectPatch::INVALID;
}
SE_2DNodeElement::~SE_2DNodeElement()
{

}
void SE_2DNodeElement::spawn()
{
    //SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    calculateRect(mPivotX, mPivotY, 0, 0);
    std::vector<SE_Element*> children = getChildren();//elementManager->getChildren(getID());
    for(int i = 0 ; i < children.size() ; i++)
    {
        children[i]->spawn();
    }
}
void SE_2DNodeElement::update(const SE_TimeKey& timeKey)
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> children = elementManager->getChildren(getID());
    for(int i = 0 ; i < children.size() ; i++)
    {
        children[i]->update(timeKey);
    }
}
static void merge(SE_Rect<float>& mergedRect ,const SE_Rect<float>& srcRect)
{
	if(srcRect.left < mergedRect.left)
		mergedRect.left = srcRect.left;
	if(srcRect.top < mergedRect.top)
		mergedRect.top = srcRect.top;
	if(srcRect.right > mergedRect.right)
		mergedRect.right = srcRect.right;
	if(srcRect.bottom > mergedRect.bottom)
		mergedRect.bottom = srcRect.bottom;
}
void SE_2DNodeElement::layout()
{
	SE_Rect<float> mergedRect;
	mergedRect.left = INVALID_GEOMINFO;
	mergedRect.top = INVALID_GEOMINFO;
	mergedRect.right = -INVALID_GEOMINFO;
	mergedRect.bottom = -INVALID_GEOMINFO;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> children = elementManager->getChildren(getID());
    for(int i = 0 ; i < children.size() ; i++)
    {
		SE_2DNodeElement* e = (SE_2DNodeElement*)children[i];
		e->layout();
        SE_Rect<float> srcRect;
		srcRect.left = e->getLeft() + e->getDeltaLeft();
		srcRect.top = e->getTop() + e->getDeltaTop();
		srcRect.right = e->getLeft() + e->getDeltaLeft() + e->getWidth();
		srcRect.bottom = e->getTop() + e->getDeltaTop() + e->getHeight();
		merge(mergedRect, srcRect);
	}
	float left = 0, top = 0;
	if(left < mergedRect.left)
		mergedRect.left = left;
	if(top < mergedRect.top)
		mergedRect.top = top;
	if(left > mergedRect.right )
		mergedRect.right = left;
	if(top > mergedRect.bottom)
		mergedRect.bottom = top;

	mDeltaLeft = mergedRect.left;
	mDeltaTop = mergedRect.top;
	mWidth = mergedRect.right - mergedRect.left;
	mHeight = mergedRect.bottom - mergedRect.top;
}

SE_Spatial* SE_2DNodeElement::createSpatial()
{
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Spatial* parent = spatialManager->createSpatial(mSpatialType);
	parent->setRenderTarget(mRenderTarget);
	parent->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	parent->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
	parent->setNeedUpdateTransform(mNeedUpdateTransform);
    parent->setPrevMatrix(mPrevMatrix);
    parent->setPostMatrix(mPostMatrix);
	parent->setLocalLayer(mLocalLayer);
	parent->setElementID(getID());
	std::vector<SE_Element*> children = elementManager->getChildren(getID());
	for(int i = 0 ; i < children.size() ; i++)
	{
		SE_Element* e = children[i];
		SE_Spatial* spatial = e->createSpatial();
		if(spatial)
		{
            spatialManager->add(parent, spatial);
		}
	}
	return parent;
}
SE_CameraID SE_2DNodeElement::createRenderTargetCamera(float left, float top, float width, float height)
{
    float ratio = height / width;
	float angle = 2 * SE_RadianToAngle(atanf(width / 20.0f));
    SE_Camera* camera = new SE_Camera;
	float cameraLeft = left + width / 2;
	float cameraTop = top + height / 2;
	SE_Vector3f v(cameraLeft , cameraTop , 10);
	camera->setLocation(v);
	camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
	camera->setViewport(0, 0, width, height);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_CameraID cameraID = cameraManager->addCamera(camera);
	return cameraID;
}

void SE_2DNodeElement::update(SE_ParamValueList& paramValueList)
{
}
void SE_2DNodeElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_ElementParamUpdateEvent* event = new SE_ElementParamUpdateEvent;
    event->mElementID = getID();
    event->mParamValueList.add(address, value);
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    elementManager->addEvent(event);    
}
void SE_2DNodeElement::updateMountPoint()
{
	SE_2DNodeElement* parent = (SE_2DNodeElement*)getParent();
	if(parent && mMountPointID.isValid())
	{
		SE_MountPoint mp = parent->getMountPoint(mMountPointID);
		mMountPointX = mp.getX();
		mMountPointY = mp.getY();
	}
}
void SE_2DNodeElement::calculateRect(float pivotx, float pivoty, float width, float height)
{
	if(width == INVALID_GEOMINFO || height == INVALID_GEOMINFO)
	{
		LOGE("... error width and height can not be INVALID_GEOMINFO\n");
		return;
	}
	if(pivotx != INVALID_GEOMINFO)
	{
        mPivotX = pivotx;
	}
	if(pivoty != INVALID_GEOMINFO)
	{
	    mPivotY = pivoty;
	}
	updateMountPoint();
	if(mPivotX == INVALID_GEOMINFO || mPivotY == INVALID_GEOMINFO ||
	   mMountPointX == INVALID_GEOMINFO || mMountPointY == INVALID_GEOMINFO)
	{
	}
	else
	{
        mLeft = mMountPointX - mPivotX;
		mTop = mMountPointY - mPivotY;
		mWidth = width;
		mHeight = height;
	}
	if(mLeft == INVALID_GEOMINFO || mTop == INVALID_GEOMINFO ||
		mWidth == INVALID_GEOMINFO || mHeight == INVALID_GEOMINFO)
	{
		LOGE("... error element geometry not correct\n");
	}


}
SE_ImageData* SE_2DNodeElement::createImageData(const SE_ImageDataID& imageDataID)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
	if(!imageData)
	{
		imageData = new SE_ImageData;
		resourceManager->setImageData(imageDataID, imageData);
	}
	return imageData;
}
SE_Spatial* SE_2DNodeElement::createNode()
{
	SE_Spatial* commonNode = new SE_CommonNode;
	commonNode->setRenderTarget(mRenderTarget);
	commonNode->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	commonNode->setLocalTranslate(SE_Vector3f(getLeft(), getTop(), 0));
	commonNode->setNeedUpdateTransform(mNeedUpdateTransform);
	return commonNode;
}
void SE_2DNodeElement::createPrimitive(SE_PrimitiveID& outID, SE_Primitive*& outPrimitive)
{
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_Primitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
	if(mRectPatchType == SE_RectPatch::INVALID)
        SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	else
		SE_RectPatch::create(rect3D, (SE_RectPatch::PATCH_TYPE)mRectPatchType, primitive, primitiveID);
	outID = primitiveID;
	outPrimitive = primitive;
}
SE_Spatial* SE_2DNodeElement::createRectPatchSpatial()
{
	return NULL;
}
SE_Spatial* SE_2DNodeElement::createSpatialByImage()
{
    SE_Primitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
	createPrimitive(primitiveID, primitive);
	setImageData(primitive);
    SE_Mesh** meshArray = NULL;
    int meshNum = 0;
    primitive->createMesh(meshArray, meshNum);
    SE_ASSERT(meshNum > 0);
	SE_ASSERT(meshArray != NULL);
    for(int i = 0 ; i < meshNum ; i++)
	{
	    for(int j = 0 ; j < meshArray[j]->getSurfaceNum(); j++)
	    {
		    SE_Surface* surface = meshArray[j]->getSurface(j);
		    setSurface(surface);
	    }
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray, meshNum, OWN);
	simObject->setName(mFullPathName.getStr());
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    SE_SimObjectID simObjectID = simObjectManager->add(simObject);
    SE_Geometry* geom = new SE_Geometry;
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    geom->setLocalScale(SE_Vector3f(mWidth / 2, mHeight / 2, 1));
    geom->setLocalLayer(mLocalLayer);
	geom->setElementID(getID());
	geom->setRenderTarget(mRenderTarget);
	geom->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
    delete[] meshArray;
	//SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	//mSpatialID = spatialManager->addSpatial()
    mPrimitiveID = primitiveID;
    mSimObjectID = simObjectID;
    return geom;
}
void SE_2DNodeElement::clone(SE_Element* src, SE_Element* dst)
{
	SE_Element::clone(src, dst);
	SE_2DNodeElement* src2D = (SE_2DNodeElement*)src;
	SE_2DNodeElement* dst2D = (SE_2DNodeElement*)dst;
    dst2D->mLeft = src2D->mLeft;
	dst2D->mTop = src2D->mTop;
    dst2D->mWidth = src2D->mWidth;
    dst2D->mHeight = src2D->mHeight;
	dst2D->mPivotX = src2D->mPivotX;
	dst2D->mPivotY = src2D->mPivotY;
    dst2D->mDeltaLeft = src2D->mDeltaLeft;
    dst2D->mDeltaTop = src2D->mDeltaTop;
	dst2D->mMountPointX = src2D->mMountPointY;
	dst2D->mMountPointY = src2D->mMountPointY;
	dst2D->mMountPointID = src2D->mMountPointID;
    dst2D->mMountPointSet = src2D->mMountPointSet;
}
SE_Element* SE_2DNodeElement::clone()
{
    SE_2DNodeElement* element = new SE_2DNodeElement;
	clone(this, element);
    return element;
}
void SE_2DNodeElement::read(SE_BufferInput& inputBuffer)
{
    mType = inputBuffer.readInt();    
    mSpatialType = inputBuffer.readInt();
    mState = inputBuffer.readInt();
    mLeft = inputBuffer.readFloat();
    mTop = inputBuffer.readFloat();
    mWidth = inputBuffer.readFloat();
    mHeight = inputBuffer.readFloat();
    mPivotX = inputBuffer.readFloat();
    mPivotY = inputBuffer.readFloat();
    mMountPointX = inputBuffer.readFloat();
    mMountPointY = inputBuffer.readFloat();
    mDeltaLeft = inputBuffer.readFloat();
    mDeltaTop = inputBuffer.readFloat();
    mKeyFrameNum = inputBuffer.readInt();
    mSeqNum = inputBuffer.readInt();
    mLocalTranslate = inputBuffer.readVector3f();
    mLocalScale = inputBuffer.readVector3f();
    mLocalRotate = inputBuffer.readQuat();
    mLocalLayer.read(inputBuffer);
    mName.read(inputBuffer);
    mFullPathName.read(inputBuffer);
    mMountPointID.read(inputBuffer);
    mTimeKey.read(inputBuffer);
    mStartKey.read(inputBuffer);
    mEndKey.read(inputBuffer);
    //mURI.read(inputBuffer);
    int hasController = inputBuffer.readInt();
    if(hasController)
    {
        if(mKeyFrameController)
            delete mKeyFrameController;
        mKeyFrameController = new SE_KeyFrameController;
        mKeyFrameController->read(inputBuffer);
    }
    else
    {
        if(mKeyFrameController)
            delete mKeyFrameController;
    }
    mMountPointSet.read(inputBuffer);
    mOwnRenderTargetCamera = (bool)inputBuffer.readInt();
    mNeedUpdateTransform = (bool)inputBuffer.readInt();
}
void SE_2DNodeElement::setImageData(SE_Primitive* primitive)
{}
void SE_2DNodeElement::setSurface(SE_Surface* surface)
{}
void SE_2DNodeElement::write(SE_BufferOutput& outputBuffer)
{
    outputBuffer.writeString("SE_2DNodeElement");
    outputBuffer.writeInt(mType);
    outputBuffer.writeInt(mSpatialType);
    outputBuffer.writeInt(mState);
    outputBuffer.writeFloat(mLeft);
    outputBuffer.writeFloat(mTop);
    outputBuffer.writeFloat(mWidth);
    outputBuffer.writeFloat(mHeight);
    outputBuffer.writeFloat(mPivotX);
    outputBuffer.writeFloat(mPivotY);
    outputBuffer.writeFloat(mMountPointX);
    outputBuffer.writeFloat(mMountPointY);
    outputBuffer.writeFloat(mDeltaLeft);
    outputBuffer.writeFloat(mDeltaTop);
    outputBuffer.writeInt(mKeyFrameNum);
    outputBuffer.writeInt(mSeqNum);
    outputBuffer.writeVector3f(mLocalTranslate);
    outputBuffer.writeVector3f(mLocalScale);
    outputBuffer.writeQuat(mLocalRotate);
    mLocalLayer.write(outputBuffer);
    mName.write(outputBuffer);
    mFullPathName.write(outputBuffer);
    mMountPointID.write(outputBuffer);
    mTimeKey.write(outputBuffer);
    mStartKey.write(outputBuffer);
    mEndKey.write(outputBuffer);
    //mURI.write(outputBuffer);
    if(mKeyFrameController)
    {
        outputBuffer.writeInt(1);
        mKeyFrameController->write(outputBuffer);
    }
    else
    {
        outputBuffer.writeInt(0);
    }
    mMountPointSet.write(outputBuffer);
    outputBuffer.writeInt(mOwnRenderTargetCamera);
    outputBuffer.writeInt(mNeedUpdateTransform);
}
