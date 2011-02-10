#include "SE_Element.h"
#include "SE_KeyFrameController.h"
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
SE_Spatial* SE_Element::createSpatial(int spatialType)
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
///////////////////////////////////////
SE_2DNodeElement::SE_2DNodeElement()
{
    mSpatialType = SE_Spatial::COMMON_NODE;
}
SE_2DNodeElement::~SE_2DNodeElement()
{

}
void SE_2DNodeElement::spawn()
{
    SE_ElementManager* elementManger = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> children = elementManager->getChildren(getID());
    for(int i = 0 ; i < children.size() ; i++)
    {
        children[i]->spawn();
    }
}
void SE_2DNodeElement::update(const SE_TimeKey& timeKey)
{
    SE_ElementManager* elementManger = SE_Application::getInstance()->getElementManager();
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
    SE_ElementManager* elementManger = SE_Application::getInstance()->getElementManager();
    std::vector<SE_Element*> children = elementManager->getChildren(getID());
    for(int i = 0 ; i < children.size() ; i++)
    {
		SE_Element* e = children[i];
		e->layout();
        SE_Rect<float> srcRect;
		srcRect.left = e->getLeft() + e->getDeltaLeft();;
		srcRect.top = e->getTop() + e->getDeltaTop();;
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
    SE_ElementManager* elementManger = SE_Application::getInstance()->getElementManager();
	SE_Spatial* parent = SE_SpatialManager::createNode(mSpatialType);
	parent->setRenderTarget(mRenderTarget);
	parent->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	parent->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
	parent->setNeedUpdateTransform(mNeedUpdateTransform);
	std::vector<SE_Element*> children = elementManager->getChildren();
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
