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
        mElement = elementManager->findElement(mElementID);
    if(!mElement)
        return;
    mElement->update(mParamValueList);
}
bool SE_ElementParamUpdateEvent::merge(SE_ElementEvent* mergeEvent)
{
    SE_ElementParamUpdateEvent* pEvent = (SE_ElementParamUpdateEvent*)mergeEvent;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(!mElement)
        mElement = elementManager->findElement(mElementID);
    SE_Element* mergedElement = elementManager->findElement(pEvent->mElementID);
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
    mLeft = mTop = mWidth = mHeight = 0;
    mPivotX = mPivotY = mMountPointX = mMountPointY = 0;
    mDeltaLeft = mDeltaTop = 0;
    mKeyFrameNum = 0;
    mSeqNum = -1;
    mKeyFrameController = NULL;
    mOwnRenderTargetCamera = false;
}
SE_Element::~SE_Element()
{
    if(mKeyFrameController)
        delete mKeyFrameController;
}
SE_Element* SE_Element::getParent()
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	return elementManager->getParent(getID());
}
void SE_Element::updateMountPoint()
{
	SE_Element* parent = getParent();
	if(parent && mMountPointID.isValid())
	{
		SE_MountPoint mp = parent->getMountPoint(mMountPointID);
		mMountPointX = mp.getX();
		mMountPointY = mp.getY();
	}
}
void SE_Element::calculateRect(float pivotx, float pivoty, float width, float height)
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
void SE_Element::setKeyFrameController(SE_KeyFrameController* kfc)
{
    if(mKeyFrameController)
        delete mKeyFrameController;
    mKeyFrameController = kfc;
}
void SE_Element::spawn()
{}
void SE_Element::update(const SE_TimeKey& timeKey)
{}
void SE_Element::layout()
{}
void SE_Element::updateSpatial()
{
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* spatial = spatialManager->findSpatial(getID());
	SE_Element* parent = getParent();
	if(!parent)
		return;
    SE_Spatial* parentSpatial = spatialManager->findSpatial(parent->getID());
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
		spatialManager->addSpatial(parentSpatial->getID(), s);
    }
    else if(spatial && parentSpatial)
    {
        SE_Spatial* oldSpatial = spatialManager->removeSpatial(mSpatialID);
        SE_Spatial* s = createSpatial();
        spatialManager->addSpatial(parentSpatial->getID(), s);
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

void SE_Element::clone(const SE_Element* srcElement)
{}
void SE_Element::read(SE_BufferInput& inputBuffer)
{

}
void SE_Element::write(SE_BufferOutput& outputBuffer)
{
}
void SE_Element::setSceneRenderSeq(const SE_SceneRenderSeq& seq)
{
    mSceneRenderSeq = seq;
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
    if(!renderTarget.isValid())
    {
        mRenderTarget = renderTarget;
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
SE_CameraID SE_Element::createRenderTargetCamera(float left, float top, float width, float height)
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
SE_ImageData* SE_Element::createImageData(const SE_ImageDataID& imageDataID)
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
///////////////////////////////////////
SE_2DNodeElement::SE_2DNodeElement()
{
    mSpatialType = SE_COMMON_NODE_TYPE;
}
SE_2DNodeElement::~SE_2DNodeElement()
{

}
void SE_2DNodeElement::spawn()
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> children = elementManager->getChildren(getID());
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
		SE_Element* e = children[i];
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
/*
SE_Spatial* SE_2DNodeElement::createSpatial(const SE_SpatialID& parentID)
{
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_ElementManager* elementManger = SE_Application::getInstance()->getElementManager();
	SE_Spatial* currSpatial = spatialManager->createSpatial(mSpatialType);
    mSpatialID = spatialManager->addSpatial(parentID, currSpatial);
	//SE_Spatial* parent = spatialManager->findSpatial(parentID);
	currSpatial->setRenderTarget(mRenderTarget);
	currSpatial->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	currSpatial->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
	currSpatial->setNeedUpdateTransform(mNeedUpdateTransform);
	std::vector<SE_Element*> children = elementManager->getChildren();
	for(int i = 0 ; i < children.size() ; i++)
	{
		SE_Element* e = children[i];
		SE_Spatial* spatial = e->createSpatial(mSpatialID);
	}
	return currSpatial;
}
*/
SE_Spatial* SE_2DNodeElement::createSpatial()
{
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Spatial* parent = spatialManager->createSpatial(mSpatialType);
	parent->setRenderTarget(mRenderTarget);
	parent->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	parent->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
	parent->setNeedUpdateTransform(mNeedUpdateTransform);
	std::vector<SE_Element*> children = elementManager->getChildren(getID());
	for(int i = 0 ; i < children.size() ; i++)
	{
		SE_Element* e = children[i];
		SE_Spatial* spatial = e->createSpatial();
		if(spatial)
		{
            spatialManager->addSpatial(parent, spatial);
		}
	}
	return parent;
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
    mURI.read(inputBuffer);
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
    mURI.write(outputBuffer);
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
