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
