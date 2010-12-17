#include "SE_Element.h"
#include "SE_Geometry3D.h"
#include "SE_Primitive.h"
#include "SE_ImageData.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageCodec.h"
#include "SE_MeshSimObject.h"
#include "SE_Geometry.h"
#include "SE_SimObjectManager.h"
#include "SE_SceneManager.h"
#include "SE_Animation.h"
#include "SE_AnimationManager.h"
#include "SE_MountPoint.h"
#include "SE_Mesh.h"
#include "SE_Image.h"
#include "SE_Log.h"
#include "SE_Action.h"
#include "SE_CommonNode.h"
#include "SE_Sequence.h"
#include "SE_ElementKeyFrameAnimation.h"
#include "SE_ColorEffectController.h"
#include "SE_DataValueDefine.h"
#include "SE_RenderTargetManager.h"
#include "SE_Camera.h"
#include "SE_RenderTarget.h"
#include "SE_RenderTargetManager.h"
#include "SE_Math.h"
#include <algorithm>
#include <math.h>
SE_Element::SE_Element()
{
    mLeft = mTop = mWidth = mHeight = INVALID_GEOMINFO;
    mAnimation = NULL;
    mParent = NULL;
    mPivotX = mPivotY = INVALID_GEOMINFO;
	mMountPointX = mMountPointY = INVALID_GEOMINFO;
	mDeltaLeft = mDeltaTop = 0;
	mActionLayer = NULL;
	mStartKey = mEndKey = 0;
	mTimeKey = 0;
	mPrevElement = NULL;
	mNextElement = NULL;
	mRenderTarget = SE_RenderTargetManager::SE_FRAMEBUFFER_TARGET;
	mSeqNum = -1;
	mNeedUpdateTransform = true;
}
SE_Element::SE_Element(float left, float top, float width, float height)
{
    mLeft = left;
    mTop = top;
    mWidth = width;
    mHeight = height;
    mAnimation = NULL;
    mParent = NULL;
    mPivotX = mPivotY = INVALID_GEOMINFO;
	mMountPointX = mMountPointY = INVALID_GEOMINFO;
	mDeltaLeft = mDeltaTop = 0;
	mActionLayer = NULL;
	mStartKey = mEndKey = 0;
	mTimeKey = 0;
	mPrevElement = NULL;
	mNextElement = NULL;
	mRenderTarget = SE_RenderTargetManager::SE_FRAMEBUFFER_TARGET;
	mSeqNum = -1;
	mCurrentContent = NULL;
	mNeedUpdateTransform = true;
}
SE_Element::~SE_Element()
{
    if(mAnimation)
		delete mAnimation;
}
int SE_Element::getKeyFrameNum()
{
	return 0;
}
void SE_Element::updateMountPoint()
{
	if(mParent && mMountPointID.isValid())
	{
		SE_MountPoint mp = mParent->getMountPoint(mMountPointID);
		mMountPointX = mp.getX();
		mMountPointY = mp.getY();
	}
}
void SE_Element::setAnimation(SE_Animation* anim)
{
	if(mAnimation)
		delete mAnimation;
	mAnimation = anim;
}
void SE_Element::addChild(SE_Element* e)
{
	mChildren.push_back(e);
	e->setParent(this);
}
void SE_Element::removeChild(SE_Element* e)
{
	if(e == NULL)
	{
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ;it++)
		{
			if(*it)
				delete *it;
		}
		mChildren.clear();
	}
	else
	{
	    mChildren.remove(e);
	    e->setParent(NULL);
	}
}
SE_Element* SE_Element::removeChild(const SE_ElementID& id)
{
	if(mChildren.empty())
		return NULL;
    _ElementList::iterator it;
    for(it = mChildren.begin() ; it != mChildren.end() ; it++)
    {
        SE_Element* e = *it;
        if(e->getID() == id)
            break;
    }
    if(it != mChildren.end())
    {
		SE_Element* e = *it;
        mChildren.erase(it);
		return e;
    }
	else
	{
		return NULL;
	}
}
SE_Spatial* SE_Element::createSpatial()
{
	if(mChildren.empty())
	{
		return NULL;
	}
	else
	{
		SE_CommonNode* commonNode = new SE_CommonNode;
		commonNode->setRenderTarget(mRenderTarget);
		mSpatialID = commonNode->getSpatialID();
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
		commonNode->setLocalTranslate(SE_Vector3f(getLeft() + getWidth() / 2, getTop() + getHeight() / 2, 0));
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			SE_Spatial* spatial = e->createSpatial();
			if(spatial)
			    commonNode->addChild(spatial);
		}
		return commonNode;
	}
}

void SE_Element::travel(SE_ElementTravel* travel)
{
	travel->visit(this);
	if(!mChildren.empty())
    {
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
            SE_Element* e = *it;
            travel->visit(e);
        }
    }
}
void SE_Element::update(unsigned int key)
{
	if(!mChildren.empty())
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
			(*it)->update(key);
		}
	}
}
void SE_Element::addMountPoint(const SE_MountPoint& mountPoint)
{
    mMountPointSet.addMountPoint(mountPoint);
}
void SE_Element::removeMountPoint(const SE_MountPointID& mountPointID)
{
	mMountPointSet.removeMountPoint(mountPointID);
}

void SE_Element::clearMountPoint()
{
	mMountPointSet.clearMountPoint();
}

SE_MountPoint SE_Element::getMountPoint(const SE_MountPointID& mountPointID)
{
	return mMountPointSet.getMountPoint(mountPointID);
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
void SE_Element::addContent(SE_ElementContent* ec)
{
	mElementContentList.push_back(ec);
}
SE_ElementContent* SE_Element::getContent(int index)
{
	_ElementContentList::iterator it = listElementRef(mElementContentList, index);
	if(it != mElementContentList.end())
	{
		return *it;
	}
	else
		return NULL;

}
void SE_Element::clearContent()
{
	for_each(mElementContentList.begin(), mElementContentList.end(), SE_DeleteObject());
	mElementContentList.clear();
}
SE_Spatial* SE_Element::createSpatialByImage(SE_ImageBase* image)
{
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	image->setImageData(primitive);
    SE_Mesh** meshArray = NULL;
    int meshNum = 0;
    primitive->createMesh(meshArray, meshNum);
    if(meshNum != 1)
	{
		LOGE("... rect primivitve mesh num should be 1\n");
		return NULL;
	}
	for(int i = 0 ; i < meshArray[0]->getSurfaceNum(); i++)
	{
		SE_Surface* surface = meshArray[0]->getSurface(i);
		image->setSurface(surface);
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
	simObject->setName(mID.getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_Geometry* geom = new SE_Geometry;
	SE_SpatialID spatialID = geom->getSpatialID();
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    geom->setLocalScale(SE_Vector3f(mWidth / 2, mHeight / 2, 1));
    geom->setLocalLayer(mLocalLayer);
	SE_ElementID eid = SE_ID::createElementID(mID.getStr());
	geom->setElementID(eid);
	geom->setRenderTarget(mRenderTarget);
    delete[] meshArray;
    mSpatialID = spatialID;
    mPrimitiveID = primitiveID;
    mSimObjectID = simObjectID;
    return geom;
}
void SE_Element::spawn()
{
	if(!mChildren.empty())
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
			(*it)->spawn();
		}
	}
	else if(!mElementContentList.empty())
	{
		_ElementContentList::iterator it;
		for(it = mElementContentList.begin() ; it != mElementContentList.end(); it++)
		{
			SE_ElementContent* ec = *it;
			SE_MountPoint mp(INVALID_GEOMINFO, INVALID_GEOMINFO);
			if(mParent)
				mp = mParent->getMountPoint(mMountPointID);
			SE_Element* e = ec->createElement(mp.getX(), mp.getY());
			this->addChild(e);
			e->spawn();
		}
	}
}
void SE_Element::clone(SE_Element *src, SE_Element* dst)
{
	dst->mLeft = src->mLeft;
	dst->mTop = src->mTop;
	dst->mWidth = src->mWidth;
	dst->mHeight = src->mHeight;
	dst->mPivotX = src->mPivotX;
	dst->mPivotY = src->mPivotY;
	dst->mMountPointX = src->mMountPointY;
	dst->mMountPointY = src->mMountPointY;
	dst->mLocalLayer = src->mLocalLayer;
	dst->mLocalTranslate = src->mLocalTranslate;
	dst->mLocalScale = src->mLocalScale;
	dst->mLocalRotate = src->mLocalRotate;
	dst->mID = src->mID;
	dst->mMountPointID = src->mMountPointID;
	dst->mElementRef = src->mElementRef;
	dst->mTimeKey = src->mTimeKey;
	dst->mStartKey = src->mStartKey;
	dst->mEndKey = src->mEndKey;
	dst->mKeyFrameNum = src->mKeyFrameNum;
	dst->mRenderTarget = src->mRenderTarget;
	dst->mSeqNum = src->mSeqNum;
	_ElementContentList::iterator it;
	for(it = mElementContentList.begin() ; it != mElementContentList.end() ; it++)
	{
		SE_ElementContent* e = (*it)->clone();
		dst->mElementContentList.push_back(e);
	}
}
SE_Element* SE_Element::clone()
{
	SE_Element* e = new SE_Element;
	clone(this, e);
	if(!mChildren.empty())
	{
		_ElementList::iterator it;
		for(it = mChildren.begin(); it != mChildren.end(); it++)
		{
			SE_Element* child = (*it)->clone();
			e->addChild(child);
		}
	}
	return e;

}
void SE_Element::merge(SE_Rect<float>& mergedRect ,const SE_Rect<float>& srcRect)
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
void SE_Element::measure()
{
	if(!mChildren.empty())
	{
		_ElementList::iterator it ;
		SE_Rect<float> mergedRect;
		mergedRect.left = INVALID_GEOMINFO;
		mergedRect.top = INVALID_GEOMINFO;
		mergedRect.right = -INVALID_GEOMINFO;
		mergedRect.bottom = -INVALID_GEOMINFO;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			e->measure();
            SE_Rect<float> srcRect;
			srcRect.left = e->getLeft() + e->getDeltaLeft();;
			srcRect.top = e->getTop() + e->getDeltaTop();;
			srcRect.right = e->getLeft() + e->getDeltaLeft() + e->getWidth();
			srcRect.bottom = e->getTop() + e->getDeltaTop() + e->getHeight();
			merge(mergedRect, srcRect);
		}
		float left = 0, right = 0;
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
}
void SE_Element::setRenderTarget(const SE_RenderTargetID& id)
{
	mRenderTarget = id;
	if(!mChildren.empty())
	{
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			e->setRenderTarget(id);
		}
	}
}
/*
void SE_Element::setCurrentContent(const SE_StringID& id)
{
	if(!mChildren.empty())
	{
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			e->setCurrentContent(id);
		}
	}
	else
	{
		_ElementContentList::iterator it;
		for(it = mElementContentList.begin() ; it != mElementContentList.end() ; it++)
		{
			SE_ElementContent* e = *it;
			if(e->getID() == id)
				break;
		}
		if(it != mElementContentList.end())
		{
			mCurrentContent = *it;
		}
	}
}
*/
void SE_Element::startAnimation()
{
	if(mAnimation)
	{
        SE_AnimationManager* animationManager = SE_Application::getInstance()->getAnimationManager();
        SE_Animation* newAnim = mAnimation->clone();
        SE_AnimationID animID = getAnimationID();
        animationManager->removeAnimation(animID);
        animID = animationManager->addAnimation(newAnim);
        setAnimationID(animID);
        newAnim->run();
	}
    _ElementList::iterator it;
    for(it = mChildren.begin() ; it != mChildren.end() ; it++)
    {
        SE_Element* e = *it;
        e->startAnimation();
    }
}
///////////////
SE_ElementContent* SE_ElementContent::clone()
{
	return NULL;
}
///////////////////////////////////////
SE_ImageContent::SE_ImageContent(const SE_StringID& imageURI) : mImageURI(imageURI)
{

}
SE_ElementContent* SE_ImageContent::clone()
{
	SE_ImageContent* e = new SE_ImageContent(mImageURI);
	e->setID(getID());
	return e;
}
SE_Element* SE_ImageContent::createElement(float mpx, float mpy)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	SE_Util::SplitStringList strList = SE_Util::splitString(mImageURI.getStr(), "/");
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	SE_Element* rete = NULL;
	if(t == SE_IMAGE_TABLE)
	{
	    SE_ImageElement* imageElement = new SE_ImageElement;
	    imageElement->setMountPoint(mpx, mpy);
	    imageElement->setImage(mImageURI);
		rete = imageElement;
	}
	else if(t == SE_ELEMENT_TABLE)
	{
		resourceManager->loadElement(strList[0].c_str());
		std::string ename = strList[0] + "/" + strList[1];
		SE_Element* e = resourceManager->getElement(ename.c_str());
        SE_TextureElement* textureElement = new SE_TextureElement;
		textureElement->addChild(e);
		textureElement->setPivotX(e->getPivotX());
		textureElement->setPivotY(e->getPivotY());
		textureElement->setMountPoint(mpx, mpy);
		SE_ImageDataID imageDataID = e->getID().getStr();
		SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
		if(!imageData)
		{
			imageData = new SE_ImageData;
			resourceManager->setImageData(imageDataID, imageData);
		}
		textureElement->setImage(imageDataID, imageData);
		e->setMountPoint(0, 0);
		e->setPivotX(0);
		e->setPivotX(0);
		SE_TextureTarget* textureTarget = new SE_TextureTarget(imageData);
		SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	    SE_RenderTargetID renderTargetID = renderTargetManager->addRenderTarget(textureTarget);
		mRenderTargetID = renderTargetID;
		e->setRenderTarget(renderTargetID);
	    e->setNeedUpdateTransform(false);
		rete = textureElement
	}
	return rete;
}
SE_TextureElement::SE_TextureElement(SE_ElementImage* image)
{
	mElementImage = image;
}
SE_TextureElement::~SE_TextureElement()
{
	if(mElementImage)
		delete mElementImage;
}
void SE_TextureElement::setElementImage(SE_ElementImage* image)
{
	if(mElementImage)
		delete mElementImage;
	mElementImage = image;
}
void SE_TextureElement::spawn()
{
	if(mElementImage)
	{
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
		if(!mChildren.empty())
		{
			_ElementList::iterator it;
			for(it = mChildren.begin() ; it != mChildren.end() ; it++)
			{
				SE_Element* e = *it;
				e->spawn();
			}
		}
	}
}
void SE_TextureElement::setImage(const SE_ImageDataID& id, SE_ImageData* imageData)
{
	if(mElementImage)
		delete mElementImage;
	mElementImage = new SE_ElementImage(id, imageData);
}
void SE_TextureElement::measure()
{
	SE_Element::measure();
	float ratio = mHeight / mWidth
	float angle = 2 * SE_RadianToAngle(atanf(mWidth / 20.0f));
    SE_Camera* camera = new SE_Camera;
	float left = mLeft + mDeltaLeft + mWidth / 2;
	float top = mTop + mDeltaTop + mHeight / 2;
	SE_Vector3f v(left , top , 10);
	camera->setLocation(v);
	camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
	camera->setViewport(0, 0, mWidth, mHeight);
	mElementImage->getImageData()->setWidth(mWidth);
	mElementImage->getImageData()->setHeight(mHeight);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	SE_TextureTarget* textureTarget = renderTargetManager->getRenderTarget(mRenderTargetID);
	textureTarget->setWidth(mWidth);
	textureTarget->setHeight(mHeight);
	textureTarget->setCamera(camera);
	textureTarget->create();
}
SE_Spatial* SE_TextureElement::createSpatial()
{
    SE_CommonNode* node = new SE_CommonNode;
    SE_Spatial* spatial = createSpatialByImage(mElementImage);
	if(spatial)
	    node->addChild(spatial);
	_ElementList::iterator it;
	for(it = mChildren.begin() ; it != mChildren.end() ; it++)
	{
		SE_Element* e = *it;
		SE_Spatial* s = e->createSpatial();
		if(s)
	        node->addChild(s);
	}
	return node;
}
/////////
SE_ActionContent::SE_ActionContent(const SE_StringID& actionURI) : mActionURI(actionURI)
{}
SE_ElementContent* SE_ActionContent::clone()
{
	SE_ActionContent* e = new SE_ActionContent(mActionURI);
	e->setID(getID());
	return e;
}
SE_Element* SE_ActionContent::createElement(float mpx, float mpy)
{
	return NULL;
}
SE_StateTableContent::SE_StateTableContent(const SE_StringID& stateTableURI) : mStateTableURI(stateTableURI)
{}
SE_ElementContent* SE_StateTableContent::clone()
{
	SE_StateTableContent* e = new SE_StateTableContent(mStateTableURI);
	e->setID(getID());
	return e;
}
SE_Element* SE_StateTableContent::createElement(float mpx, float mpy)
{
	return NULL;
}
////////////
SE_Spatial* SE_NullElement::createSpatial()
{
	SE_Spatial* spatial = new SE_Spatial;
	spatial->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    spatial->setLocalLayer(mLocalLayer);
	setSpatialID(spatial->getSpatialID());
	return spatial;
}
//////////////////////////////////////////////////////////////////////////
SE_ImageElement::SE_ImageElement()
{
	mImage = NULL;
}
SE_ImageElement::~SE_ImageElement()
{
	if(mImage)
		delete mImage;
}
void SE_ImageElement::spawn()
{
	SE_StringID imageDataID = mImageID;
	if(mImage)
	{
	    setRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
		return;
	}
	if(imageDataID.isValid())
	{
	    mImage = new SE_Image(imageDataID.getStr());
	}
	if(mImage)
	{
	    setRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
	}
	else
	{
		setLeft(0);
		setTop(0);
		setWidth(0);
		setHeight(0);
	}
}
void SE_ImageElement::measure()
{
}
SE_Spatial* SE_ImageElement::createSpatial()
{
	if(mImage)
	{
	    return createSpatialByImage(mImage);
	}
	else
		return NULL;

}
/////////////////////////
SE_ActionElement::SE_ActionElement()
{
	mAction = NULL;
}
void SE_ActionElement::spawn()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mAction = resourceManager->getAction(mActionID.getStr());
	if(!mAction)
		return;
	mAction->sort();
	calculateRect(mAction->getPivotX(), mAction->getPivotY(), 0, 0);
	mAction->createElement(this);
}
SE_Spatial* SE_ActionElement::createSpatial()
{
	if(!mAction)
		return NULL;
    SE_CommonNode* node = new SE_CommonNode;
	node->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    node->setLocalLayer(mLocalLayer);
	setSpatialID(node->getSpatialID());
	_ElementList::iterator it;
	for(it = mChildren.begin() ; it != mChildren.end() ; it++)
	{
		SE_Element* e = *it;
		SE_Spatial* spatial = e->createSpatial();
		if(spatial)
		    node->addChild(spatial);
	}
	return node;
}
void SE_ActionElement::update(unsigned int key)
{
	_HeadElementList::iterator it;
    for(it = mHeadElementList.begin() ; it != mHeadElementList.end() ; it++)
	{
		SE_Element* e = *it;
		SE_Element* first = NULL;
		SE_Element* second = NULL;
		while(e->getTimeKey() < key)
		{
			e = e->getNext();
		}
		if(e != NULL)
		{
			if(e->getTimeKey() == key)
			{
				first = second = e;
			}
			else
			{
			    first = e->getPrev();
			    second = e;
			}
		}
		else
		{
			LOGI("... element's key are less than input key\n");
			first = e;
			second = NULL;
		}
		if(first == NULL)
		{
			LOGI("... input key is less than first element's key");
		}
		else
		{
            //calculate current element by controller
			first->update(key - first->getTimeKey());
		}
	}
}
//////////////
SE_SequenceElement::SE_SequenceElement(SE_Sequence* sequence)
{
	mSequence = sequence;
	mCurrentElement = NULL;
}
int SE_SequenceElement::getKeyFrameNum()
{
	if(!mSequence)
		return 0;
    std::vector<unsigned int> keys = mSequence->getKeys();
    return keys.size();
}
void SE_SequenceElement::spawn()
{
	calculateRect(mSequence->getPivotX(), mSequence->getPivotY(), 0, 0);
    mMountPointSet.clearMountPoint();
	std::vector<SE_MountPoint> mountPoints = mSequence->getMountPoint();
	for(int i = 0 ; i < mountPoints.size(); i++)
	{
		mMountPointSet.addMountPoint(mountPoints[i]);
	}
	std::vector<unsigned int> keys = mSequence->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_Sequence::_Frame f = mSequence->getFrame(keys[i]);
		SE_ImageElement* e = new SE_ImageElement;
		e->setMountPointRef(f.mpref);
		e->setImage(f.imageref);
		e->setTimeKey(keys[i]);
		e->setParent(this);
		this->addChild(e);
		e->spawn();
	}
	SE_ElementKeyFrameAnimation* anim = new SE_ElementKeyFrameAnimation;
	anim->setElement(this);
	int frameRate = SE_Application::getInstance()->getFrameRate();
	SE_TimeMS duration = (this->getEndKey() - this->getStartKey()) * frameRate;
	anim->setDuration(duration);
	this->setAnimation(anim);
}
SE_Spatial* SE_SequenceElement::createSpatial()
{
	if(!mSequence)
		return NULL;
    SE_CommonNode* node = new SE_CommonNode;
	node->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    node->setLocalLayer(mLocalLayer);
	setSpatialID(node->getSpatialID());
	return node;
}
void SE_SequenceElement::update(unsigned int key)
{
	if(mChildren.empty())
		return;
	_ElementList::iterator it;
	SE_Element* first = NULL;
	SE_Element* second = NULL;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	_ElementList::iterator currIt = mChildren.end();
	for(it = mChildren.begin() ; it != mChildren.end() ; it++)
	{
		SE_Element* e = *it;
		if(e->getTimeKey() >= key)
		{
            currIt = it;
			break;
		}
	}
	if(currIt == mChildren.end())
	{
		currIt--;
		first = *currIt;
		second = NULL;
	}
	else
	{
		if(currIt == mChildren.begin())
		{
			if((*currIt)->getTimeKey() == key)
			{
				first = second = *currIt;
			}
			else
			{
			    first = NULL;
			    second = *currIt;
			}
		}
		else
		{
			second = *currIt;
			currIt--;
			first = second;
		}
	}
	if(first == NULL)
	{
		LOGI("... current key is less than element key");
	}
	else
	{
		if(mCurrentElement)
		{
			simObjectManager->remove(mCurrentElement->getSimObjectID());
			resourceManager->removePrimitive(mCurrentElement->getPrimitiveID());
			SE_Spatial* spatial = sceneManager->removeSpatial(mCurrentElement->getSpatialID());
			if(spatial)
				delete spatial;
		}
		SE_Spatial* spatial = first->createSpatial();
		SE_Spatial* parentSpatial = sceneManager->find(getSpatialID());
		sceneManager->addSpatial(parentSpatial, spatial);
		spatial->updateRenderState();
		spatial->updateWorldLayer();
		spatial->updateWorldTransform();
		mCurrentElement = first;
	}
}
////////////////
void SE_StateTableElement::update(unsigned int key)
{}
//////////////
SE_ColorEffectImageElement::SE_ColorEffectImageElement(SE_ColorEffectImage* image) : mColorEffectImage(image)
{}
SE_ColorEffectImageElement::~SE_ColorEffectImageElement()
{
	if(mColorEffectImage)
		delete mColorEffectImage;
}
void SE_ColorEffectImageElement::spawn()
{
    if(!mColorEffectImage)
		return;
    
}
SE_Spatial* SE_ColorEffectImageElement::createSpatial()
{
	return NULL;
}
void SE_ColorEffectImageElement::update(unsigned int key)
{}
///////////////////
void SE_ColorEffectElement::update(unsigned int key)
{
}
SE_Spatial* SE_ColorEffectElement::createSpatial()
{
	return NULL;
}
void SE_ColorEffectElement::spawn()
{
	if(!mColorEffectController)
		return;
	calculateRect(mColorEffectController->getPivotX(), mColorEffectController->getPivotY(), 0, 0);
	std::vector<unsigned int> keys = mColorEffectController->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_ColorEffectFrame* f = mColorEffectController->getKeyFrame(keys[i]);
        SE_Element* e = f->createElement(mColorEffectInput);
		e->setPivotX(f->getPivotX());
		e->setPivotY(f->getPivotY());
		e->setMountPointRef(f->getMountPointRef());
		this->addChild(e);
		e->spawn();
	}
}
