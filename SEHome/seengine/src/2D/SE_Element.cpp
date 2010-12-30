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
#include "SE_CameraManager.h"
#include "SE_ParamManager.h"
#include "SE_ElementManager.h"
#include "SE_Math.h"
#include "SE_Utils.h"
#include <algorithm>
#include <math.h>
SE_Element::SE_Element()
{
    mLeft = mTop = mWidth = mHeight = 0;
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
	mOwnRenderTargetCamera = false;
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
	mNeedUpdateTransform = true;
	mOwnRenderTargetCamera = false;
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
		commonNode->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
		mSpatialID = commonNode->getSpatialID();
		commonNode->setLocalTranslate(SE_Vector3f(getLeft(), getTop(), 0));
		commonNode->setNeedUpdateTransform(mNeedUpdateTransform);
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			SE_Spatial* spatial = e->createSpatial();
			spatial->setNeedUpdateTransform(e->mNeedUpdateTransform);
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
void SE_Element::createPrimitive(SE_PrimitiveID& outID, SE_RectPrimitive*& outPrimitive)
{
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	outID = primitiveID;
	outPrimitive = primitive;
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
	geom->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
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
			e->setLocalLayer(mLocalLayer);
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
	dst->mMountPointSet = mMountPointSet;
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
	SE_CameraID cameraID = SE_ID::createCameraID();
	cameraManager->setCamera(cameraID, camera);
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
SE_StringID SE_Element::resolveParamString(const char* str)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(str, "/");
	SE_Util::SplitStringList::iterator it;
	for(it = strList.begin() ; it != strList.end() ; it++)
	{
		std::string s = *it;
        std::string::size_type posStart = s.find("[");
		if(posStart != std::string::npos)
		{
            std::string::size_type posEnd = s.find("]");
			if(posEnd == std::string::npos)
			{
				LOGE("... [ has no ]\n");
				return "";
			}
			else
			{
                std::string::size_type n = posEnd - posStart - 1;
	            std::string subString = s.substr(posStart + 1, n);
                SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	            bool ok = false;
				std::string value = paramManager->getString(subString.c_str(), ok);
	            if(ok)
				{
					*it = value;
				}
			}
		}
	}
	it = strList.begin();
	std::string retStr = *it;
	it++;
	for(; it != strList.end() ; it++)
	{
		retStr += "/";
		retStr += *it;
	}
	return retStr.c_str();
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
		imageElement->setMountPoint(mpx, mpy);
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
		SE_ImageDataID imageDataID = e->getFullPathID().getStr();
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
		textureElement->saveRenderTargetID(renderTargetID);
		textureTarget->setBackground(SE_Vector4f(0, 0, 0, 0));
        e->setOwnRenderTargetCamera(true);
		//e->setRenderTarget(renderTargetID);
	    //e->setNeedUpdateTransform(false);
		rete = textureElement;
	}
	return rete;
}
SE_TextureElement::SE_TextureElement(SE_RawImage* image)
{
	mElementImage = image;
}
SE_TextureElement::~SE_TextureElement()
{
	if(mElementImage)
		delete mElementImage;
	SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
	rm->removeRenderTarget(mRenderTargetID);
}
void SE_TextureElement::setElementImage(SE_RawImage* image)
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
	mElementImage = new SE_RawImage(id, imageData);
}
void SE_TextureElement::measure()
{
	SE_Element::measure();
	_ElementList::iterator it;
	for(it = mChildren.begin() ; it != mChildren.end() ;it++)
	{
		SE_Element* e = *it;
		e->setRenderTarget(mRenderTargetID);
	}
	float ratio = mHeight / mWidth;
	float angle = 2 * SE_RadianToAngle(atanf(mWidth / 20.0f));
    SE_Camera* camera = new SE_Camera;
	float left = mDeltaLeft + mWidth / 2;
	float top = mDeltaTop + mHeight / 2;
	SE_Vector3f v(left , top , 10);
	camera->setLocation(v);
	camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
	camera->setViewport(0, 0, mWidth, mHeight);
	mElementImage->getImageData()->setWidth(mWidth);
	mElementImage->getImageData()->setHeight(mHeight);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_CameraID cameraID = SE_ID::createCameraID();
	cameraManager->setCamera(cameraID, camera);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	SE_RenderTarget* textureTarget = renderTargetManager->getRenderTarget(mRenderTargetID);
	textureTarget->setWidth(mWidth);
	textureTarget->setHeight(mHeight);
	textureTarget->setCamera(cameraID);
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
		e->setLeft(mLeft);
		e->setTop(mTop);
		SE_Spatial* s = e->createSpatial();
		//SE_Vector3f translate = s->getLocalTranslate();
		//SE_Vector3f texTranslate = spatial->getLocalTranslate();
		//translate = SE_Vector3f(translate.x + texTranslate.x - mWidth / 2, translate.y + texTranslate.y - mHeight / 2, translate.z) ;
		//s->setLocalTranslate(translate);
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
	SE_ActionElement* e = new SE_ActionElement;
	e->setActionID(mActionURI);
	e->setMountPoint(mpx, mpy);
	return e;
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
	    calculateRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
		return;
	}
	if(imageDataID.isValid())
	{
	    mImage = new SE_Image(imageDataID.getStr());
	}
	if(mImage)
	{
	    calculateRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
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
	_ElementList::iterator it;
	for(it = mChildren.begin() ; it != mChildren.end() ; it++)
	{
		SE_Element* e = *it;
		e->spawn();
	}
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
	node->setLocalTranslate(SE_Vector3f(mLeft , mTop , 0));
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
/*
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
*/
///////////////////
void SE_ColorEffectControllerElement::update(unsigned int key)
{
}
SE_Spatial* SE_ColorEffectControllerElement::createSpatial()
{
	return NULL;
}
void SE_ColorEffectControllerElement::spawn()
{
	if(!mColorEffectController)
		return;
	calculateRect(mColorEffectController->getPivotX(), mColorEffectController->getPivotY(), 0, 0);
	std::vector<unsigned int> keys = mColorEffectController->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_ColorEffectFrame* f = mColorEffectController->getKeyFrame(keys[i]);
        SE_Element* e = f->createElement();
		e->setPivotX(f->getPivotX());
		e->setPivotY(f->getPivotY());
		e->setMountPointRef(f->getMountPointRef());
		this->addChild(e);
		e->spawn();
	}
}
SE_ColorEffectControllerElement::~SE_ColorEffectControllerElement()
{}
///////////////
SE_ColorEffectElement::SE_ColorEffectElement()
{
	mBackgroundElement = NULL;
	mChannelElement = NULL;
	for(int i = 0 ; i < MARK_NUM ; i++)
		mTextureElement[i] = NULL;
	mBackgroundArity = 0;
	mChannelArity = 0;
}
SE_ColorEffectElement::~SE_ColorEffectElement()
{
	if(mBackgroundElement)
		delete mBackgroundElement;
	if(mChannelElement)
		delete mChannelElement;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		if(mTextureElement[i])
			delete mTextureElement[i];
	}

}
void SE_ColorEffectElement::update(unsigned int key)
{}
SE_ImageElement* SE_ColorEffectElement::createImageElement(const SE_StringID& textureURL, SE_ImageData*& imageData)
{
    SE_ImageElement* imageElement = new SE_ImageElement;// need be delete
	imageElement->setImage(textureURL);
	imageElement->spawn();
	imageElement->measure();
	imageData = createImageData(textureURL);
    imageData->setWidth(imageElement->getWidth());
	imageData->setHeight(imageElement->getHeight());
	SE_TextureTarget* renderTarget = new SE_TextureTarget(imageData);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
	SE_RenderTargetID renderTargetID = renderTargetManager->addRenderTarget(renderTarget);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	elementManager->addRenderTargetElement(imageElement);
	imageElement->setRenderTarget(renderTargetID);
    SE_CameraID cameraID = createRenderTargetCamera(imageElement->getDeltaLeft(), 
		                                            imageElement->getDeltaTop(), 
												    imageElement->getWidth(),
												    imageElement->getHeight());
	renderTarget->setCamera(cameraID);
	renderTarget->setCameraType(SE_RenderTarget::GLOBAL_CAMERA);
	renderTarget->create();
	return imageElement;
}
bool SE_ColorEffectElement::isTextureEnd(_ElementList::iterator textureIt[4], SE_Element* texture[4])
{
	bool ret = false;
	for(int i = 0 ; i < 4 ; i++)
	{
		if(texture[i])
		{
			ret = textureIt[i] == texture[i]->getChildren().end();
			if(ret)
				break;
		}
	}
	return ret;
}
SE_Element* SE_ColorEffectElement::mergeElement(SE_Element* background, SE_Element* channel, 
												SE_Element* texture[4])
{
	SE_Element* element = NULL;
    if(!background->getChildren().empty() && !channel->getChildren().empty())
	{
		element = background->clone();
		_ElementList::iterator backgroundIt = background->getChildren().begin();
		_ElementList::iterator channelIt = channel->getChildren().begin();
		_ElementList::iterator textureIt[4];
		for(int i = 0 ; i < 4 ; i++)
		{
			if(texture[i])
				textureIt[i] = texture[i]->getChildren().begin();
		}
		while(backgroundIt != background->getChildren().end() &&
			channelIt != channel->getChildren().end() && !isTextureEnd(textureIt, texture))
		{
			SE_Element* bc = *backgroundIt;
			SE_Element* cc = *channelIt;
			SE_Element* tc[4] = {NULL, NULL, NULL, NULL};
			for(int i = 0 ; i < 4 ; i++)
			{
				if(texture[i])
					tc[i] = *textureIt[i];
			}
			SE_Element* child = mergeElement(bc, cc, tc);
			element->addChild(child);
			backgroundIt++;
			channelIt++;
			for(int i = 0 ; i < 4 ; i++)
			{
				if(texture[i])
					textureIt[i]++;
			}
		}
		SE_ASSERT(backgroundIt == background->getChildren().end() && channelIt == channel->getChildren().end());
		for(int i = 0 ; i < 4 ; i++)
		{
			if(texture[i])
				SE_ASSERT(textureIt[i] == texture[i]->getChildren().end());
		}
		return element;
 	}
	else // has no child it must be SE_ImageElement
	{
		SE_ASSERT(background->getChildren().empty() && channel->getChildren().empty());
		for(int i = 0 ; i < 4 ; i++)
		{
			if(texture[i])
				SE_ASSERT(texture[i]->getChildren().empty());
		}
        SE_StringID backgroundImageURI = background->getImageURI();
		SE_StringID channelImageURI = channel->getImageURI();
		SE_StringID textureURI[4];
		for(int i = 0 ; i < 4 ; i++)
		{
			if(texture[i])
				textureURI[i] = texture[i]->getImageURI();
		}
		SE_ColorEffectElement* retElement = new SE_ColorEffectElement;
		clone(background, retElement);
		retElement->setBackgroundValue(backgroundImageURI);
		retElement->setChannelValue(channelImageURI);
		retElement->setBackgroundAlphaValue(mBackgroundAlphaValue);
		retElement->setBackgroundAlphaAddress(mBackgroundAlphaAddress);
		for(int i = 0 ; i < 4 ; i++)
		{
		    _TextureMark tm;
		    tm.mTextureValue = textureURI[i];
			tm.mColorAlphaAddress = mTextureMark[i].mColorAlphaAddress;
			tm.mColorAlphaValue = mTextureMark[i].mColorAlphaValue;
			tm.mFnAddress = mTextureMark[i].mFnAddress;
			tm.mFnValue = mTextureMark[i].mFnValue;
			tm.mTextureFnAddress = mTextureMark[i].mTextureFnAddress;
			tm.mTextureFnValue = mTextureMark[i].mTextureFnValue;
			tm.mColorAddress = mTextureMark[i].mColorAddress;
			tm.mColorValue= mTextureMark[i].mColorValue;
			retElement->setTextureMark(i, tm);
		}
		return retElement;
	}
}

void SE_ColorEffectElement::mergeElement()
{
    if(!mBackgroundElement)
		return;
	SE_Element* textureElement[4] = {mTextureImageElement[0], mTextureImageElement[1], mTextureImageElement[2],
	                                 mTextureImageElement[3]};
    mMergedElement = mergeElement(mBackgroundImageElement, mChannelImageElement, textureElement);       
	//addChild(element);
}
void SE_ColorEffectElement::measure()
{
	if(mBackgroundElement)
	{
		mBackgroundElement->measure();
		int width = mBackgroundElement->getWidth();
		int height = mBackgroundElement->getHeight();
		setWidth(width);
		setHeight(height);
	}
	else
	{
		LOGI("... color effect width = %f, height = %f", mWidth, mHeight);
	}
	if(mBackgroundArity > 1)
	{
	    mBackgroundImageDataID = mBackgroundValue.getStr();
        mBackgroundImageElement = createImageElement(mBackgroundValue, mBackgroundImageData);
	}
	if(mChannelArity > 1)
	{
		mChannelImageDataID = mChannelValue.getStr();
		mChannelImageElement = createImageElement(mChannelImageDataID, mChannelImageData);
	}
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		if(mTextureMark[i].mTextureArity > 1)
		{
			mTextureImageDataID[i] = mTextureMark[i].mTextureValue;
			mTextureImageElement[i] = createImageElement(mTextureImageDataID[i], mTextureImageData[i]);
		}
	}

}
void SE_ColorEffectElement::setSurface(SE_Surface* surface)
{

}
SE_Spatial* SE_ColorEffectElement::createSpatial()
{
	if(mBackgroundElement)
	{
        SE_Spatial* spatial = mMergedElement->createSpatial();
		return spatial;
	}
	else
	{
        SE_PrimitiveID primitiveID;
		SE_RectPrimitive* primitive;
		createPrimitive(primitiveID, primitive);
        setImageData(primitive);
        SE_Mesh** meshArray = NULL;
		int meshNum = 0;
		primitive->createMesh(meshArray, meshNum);
		if(meshNum != 1)
		{
			LOGE("... rect primivitve mesh num should be 1\n");
			delete primitive;
			return NULL;
		}
		for(int i = 0 ; i < meshArray[0]->getSurfaceNum(); i++)
		{
			SE_Surface* surface = meshArray[0]->getSurface(i);
			setSurface(surface);
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
		geom->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
		delete[] meshArray;
		mSpatialID = spatialID;
		mPrimitiveID = primitiveID;
		mSimObjectID = simObjectID;
		return geom;
	}
}
void SE_ColorEffectElement::setImageData(SE_RectPrimitive* primitive, SE_ImageData* imageData, SE_TEXUNIT_TYPE texType)
{
	SE_ImageDataPortion dp;
	dp.setX(0);
	dp.setY(0);
	dp.setWidth(imageData->getWidth());
	dp.setHeight(imageData->getHeight());
    primitive->setImageData(imageData, texType, NOT_OWN, dp);
}
void SE_ColorEffectElement::setImageData(SE_RectPrimitive* primitive, const SE_StringID& imageID, SE_TEXUNIT_TYPE texType)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageUnit iu = resourceManager->getImageUnit(imageID.getStr());
	SE_ImageData* imageData = resourceManager->getImageData(iu.imageDataID);
	SE_ImageDataPortion dp;
	dp.setX(iu.imageRect.x);
	dp.setY(iu.imageRect.y);
	dp.setWidth(iu.imageRect.width);
	dp.setHeight(iu.imageRect.height);
	primitive->setImageData(imageData, texType, NOT_OWN, dp);
}
void SE_ColorEffectElement::setImageData(SE_RectPrimitive* primitive)
{
    if(mBackgroundArity > 1)
	{
		setImageData(primitive, mBackgroundImageData, SE_TEXTURE0);
	}
	else if(mBackgroundArity == 1)
	{
        setImageData(primitive, mBackgroundValue, SE_TEXTURE0);
	}
	if(mChannelArity > 1)
	{
		setImageData(primitive, mChannelImageData, SE_TEXTURE1);
	}
	else if(mChannelArity == 1)
	{
		setImageData(primitive, mChannelValue, SE_TEXTURE1);
	}
	int start = SE_TEXTURE2;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        if(mTextureMark[i].mTextureArity > 1)
		{
			setImageData(primitive, mTextureImageData[i], (SE_TEXUNIT_TYPE)start);
		}
		else if(mTextureMark[i].mTextureArity == 1)
		{
			setImageData(primitive, mTextureMark[i].mTextureValue, (SE_TEXUNIT_TYPE)start);
		}
		start++;
	}
}
void SE_ColorEffectElement::calculateValue()
{
	SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	if(mBackgroundAddress.isValid())
	{
		bool ok = false;
		std::string str = paramManager->getString(mBackgroundAddress, ok);
		if(ok)
		{
			mBackgroundValue = str.c_str();
		}
	}
	if(mChannelAddress.isValid())
	{
        bool ok = false;
		std::string str = paramManager->getString(mChannelAddress, ok);
		if(ok)
		{
			mChannelValue = str.c_str();
		}
	}
	if(mBackgroundAlphaAddress.isValid())
	{
		bool ok = false;
		int a = paramManager->getInt(mBackgroundAlphaAddress, ok);
		if(ok)
		{
			mBackgroundAlphaValue = a;
		}
	}
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		_TextureMark& tm = mTextureMark[i];
		if(tm.mColorAddress.isValid())
		{
			bool ok = false;
			std::string str = paramManager->getString(tm.mColorAddress.getStr(), ok);
			if(ok)
			{
				tm.mColorValue = SE_Util::stringToSignColor(str.c_str());
			}
		}
		if(tm.mColorAlphaAddress.isValid())
		{
			bool ok = false;
			int v = paramManager->getInt(tm.mColorAlphaAddress, ok);
			if(ok)
			{
				tm.mColorAlphaValue = v;
			}
		}
		if(tm.mFnAddress.isValid())
		{
			bool ok = false;
			int v = paramManager->getInt(tm.mFnAddress, ok);
            if(ok)
			{
				tm.mFnValue = v;
			}
		}
		if(tm.mTextureFnAddress.isValid())
		{
			bool ok = false;
			int v = paramManager->getInt(tm.mTextureFnAddress, ok);
			if(ok)
			{
				tm.mTextureFnValue = v;
			}
		}
		if(tm.mTextureAddress.isValid())
		{
			bool ok = false;
			std::string str = paramManager->getString(tm.mTextureAddress, ok);
			if(ok)
			{
				tm.mTextureValue = str.c_str();
			}
		}
	}

}
SE_XMLTABLE_TYPE SE_ColorEffectElement::getBackgroundType()
{
	SE_ExtractImageStr backgroundExtractImage = SE_Util::stringToExtractImage(mBackgroundValue.getStr());
	if(backgroundExtractImage.base == "")
		return SE_IMAGE_TABLE;
	SE_Util::SplitStringList strList = SE_Util::splitString(backgroundExtractImage.base.c_str(), "/");
    SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	return t;
}
void SE_ColorEffectElement::getExtractImageProperty(SE_XMLTABLE_TYPE& t, int& width, int& height)
{
    SE_ExtractImageStr backgroundExtractImage = SE_Util::stringToExtractImage(mBackgroundValue.getStr());
	SE_Util::SplitStringList strList = SE_Util::splitString(backgroundExtractImage.base.c_str(), "/");
	SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	if(backgroundExtractImage.base == "")
	{
		t = SE_IMAGE_TABLE;
	}
	else
	{
	    t = resourceManager->getXmlType(strList[0].c_str());
	}
	std::string imageURL;
	if(backgroundExtractImage.base == "")
	{
		imageURL = backgroundExtractImage.red;
	}
	else
		imageURL = backgroundExtractImage.base;
	SE_ImageUnit iu = resourceManager->getImageUnit(imageURL.c_str());
	width = iu.imageRect.width;
	height = iu.imageRect.height;
	mBackgroundArity = backgroundExtractImage.getImageNum();
	SE_ExtractImageStr channelExtractImage = SE_Util::stringToExtractImage(mChannelValue.getStr());
	mChannelArity = channelExtractImage.getImageNum();
	for(int i = 0 ; i < MARK_NUM  ; i++)
	{
		_TextureMark& tm = mTextureMark[i];
		SE_ExtractImageStr textureExtractImage = SE_Util::stringToExtractImage(tm.mTextureValue.getStr());
		tm.mTextureArity = textureExtractImage.getImageNum();
	}
}
void SE_ColorEffectElement::spawn()
{
	calculateValue();
	SE_XMLTABLE_TYPE t;
	int width = 0, height = 0;
	getExtractImageProperty(t, width, height);
	mBackgroundType = t;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    if(t == SE_IMAGE_TABLE)
	{
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, width, height);
	}
	else if(t == SE_ELEMENT_TABLE)
	{
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
		if(mBackgroundElement)
			delete mBackgroundElement;
		mBackgroundElement = resourceManager->getElement(mBackgroundValue.getStr());
		mBackgroundElement->setRect(0, 0, 0, 0);
		mBackgroundElement->spawn();
		if(mChannelElement)
			delete mChannelElement;
		mChannelElement = resourceManager->getElement(mChannelValue.getStr());
        mChannelElement->setRect(0, 0, 0, 0);
		mChannelElement->spawn();
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			_TextureMark& tm = mTextureMark[i];
			if(tm.mTextureValue.isValid())
			{
			    if(mTextureElement[i])
				    delete mTextureElement[i];
				mTextureElement[i] = resourceManager->getElement(tm.mTextureValue.getStr());
				mTextureElement[i]->setRect(0, 0, 0, 0);
				mTextureElement[i]->spawn();
			}
		}
        mergeElement();
	}
}
