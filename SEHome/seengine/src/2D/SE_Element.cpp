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
#include "SE_ShaderProperty.h"
#include "SE_DataValueDefine.h"
#include "SE_StateTable.h"
#include "SE_KeyFrameController.h"
#include "SE_MessageEventCommandDefine.h"
#include <algorithm>
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
        mElement = elementManager->findByID(mElementID);
    if(!mElement)
        return;
    mElement->update(mParamValueList);
}
bool SE_ElementParamUpdateEvent::merge(SE_ElementEvent* mergeEvent)
{
    SE_ElementParamUpdateEvent* pEvent = (SE_ElementParamUpdateEvent*)mergeEvent;
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(!mElement)
        mElement = elementManager->findByID(mElementID);
    SE_Element* mergedElement = elementManager->findByID(pEvent->mElementID);
    if(!mElement || !mergedElement)
        return true;
    if(mElementID == pEvent->mElementID)
    {
        mergeEvent->mParamValueList.add(mParamValueList);
        return true;
    }
    else
        return false;
}
//////////////////////////
SE_Element::SE_Element(float left, float top, float width, float height)
{
    mLeft = left; 
    mTop = top;
    mWidth = width;
    mHeight = height;
    mPivotX = mPivotY = INVALID_GEOMINFO;
	mMountPointX = mMountPointY = INVALID_GEOMINFO;
    mDeltaLeft = 0;
    mDeltaTop = 0;
    mAnimation = NULL;
    mParent = NULL;
	mDeltaLeft = mDeltaTop = 0;
	mStartKey = mEndKey = 0;
	mTimeKey = 0;
	mPrevElement = NULL;
	mNextElement = NULL;
	mRenderTarget = SE_RenderTargetManager::SE_FRAMEBUFFER_TARGET;
	mSeqNum = -1;
	mNeedUpdateTransform = true;
	mOwnRenderTargetCamera = false;
	mKeyFrameController = NULL;
    mKeyFrameNum = 0;
    mID = SE_ID::createElementID();
}
SE_Element::~SE_Element()
{
    if(mAnimation)
		delete mAnimation;
    if(mKeyFrameController)
        delete mKeyFrameController;
    std::vector<SE_AddressID> addressV = mURI.getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->unregisterObserver(addressV[i], this);
        }
    } 
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->remove(mSimObjectID);
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Spatial* s = sceneManager->removeSpatial(mSpatialID);
    if(s != NULL)
        delete s;
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->removePrimitive(mPrimitiveID);
    SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
    animManager->removeAnimation(mAnimationID);
    _ElementContentList::iterator itContent;
    for(itContent = mElementContentList.begin() ; itContent != mElementContentList.end() ; itContent++)
    {
        SE_ElementContent* c = *itContent;
        delete c;
    }
    _ElementList::iterator it;
    for(it = mChildren.begin() ; it != mChildren.end() ; it++)
    {
        SE_Element* e = *it;
        delete e;
    }
}
int SE_Element::getKeyFrameNum() const
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
void SE_Element::setImageData(SE_RectPrimitive* primitive)
{}
void SE_Element::setSurface(SE_Surface* surface)
{}
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
void SE_Element::setURI(const SE_StringID& uri)
{
    mURI.setURI(uri);
    std::vector<SE_AddressID> addressV = mURI.getAddress();
    if(addressV.size() > 0)
    {
        SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
        for(int i = 0 ; i < addressV.size() ; i++)
        {
            paramManager->registerObserver(addressV[i], this);
        }
    } 
}
SE_Spatial* SE_Element::createNode()
{
	SE_CommonNode* commonNode = new SE_CommonNode;
	commonNode->setRenderTarget(mRenderTarget);
	commonNode->setOwnRenderTargetCamera(mOwnRenderTargetCamera);
	mSpatialID = commonNode->getSpatialID();
	commonNode->setLocalTranslate(SE_Vector3f(getLeft(), getTop(), 0));
	commonNode->setNeedUpdateTransform(mNeedUpdateTransform);
	return commonNode;
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
			if(spatial)
			{
			    spatial->setNeedUpdateTransform(e->mNeedUpdateTransform);
			    commonNode->addChild(spatial);
			}
		}
		return commonNode;
	}
}

void SE_Element::travel(SE_ElementTravel* travel) const
{
	travel->visit(const_cast<SE_Element*>(this));
	if(!mChildren.empty())
    {
        _ElementList::const_iterator it;
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

SE_MountPoint SE_Element::getMountPoint(const SE_MountPointID& mountPointID) const
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
SE_ElementContent* SE_Element::getContent(int index) const
{
	_ElementContentList::const_iterator it = listElementRef(mElementContentList, index);
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
SE_Spatial* SE_Element::createSpatialByImage()
{
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
	createPrimitive(primitiveID, primitive);
	setImageData(primitive);
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
		setSurface(surface);
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
	simObject->setName(mFullPathName.getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_Geometry* geom = new SE_Geometry;
	SE_SpatialID spatialID = geom->getSpatialID();
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    geom->setLocalScale(SE_Vector3f(mWidth / 2, mHeight / 2, 1));
    geom->setLocalLayer(mLocalLayer);
	geom->setElementID(mID);
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
	else
	{
		//calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
		if(!mElementContentList.empty())
		{
			_ElementContentList::iterator it;
			for(it = mElementContentList.begin() ; it != mElementContentList.end(); it++)
			{
				SE_ElementContent* ec = *it;
				ec->setParent(this);
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

}
int SE_Element::getContentNum() const
{
	return mElementContentList.size();
}
void SE_Element::clone(SE_Element *src, SE_Element* dst)
{
    dst->mID = SE_ID::createElementID();
	dst->mLeft = src->mLeft;
	dst->mTop = src->mTop;
	dst->mWidth = src->mWidth;
	dst->mHeight = src->mHeight;
	dst->mPivotX = src->mPivotX;
	dst->mPivotY = src->mPivotY;
    dst->mDeltaLeft = src->mDeltaLeft;
    dst->mDeltaTop = src->mDeltaTop;
	dst->mMountPointX = src->mMountPointY;
	dst->mMountPointY = src->mMountPointY;
	dst->mLocalLayer = src->mLocalLayer;
	dst->mLocalTranslate = src->mLocalTranslate;
	dst->mLocalScale = src->mLocalScale;
	dst->mLocalRotate = src->mLocalRotate;
	dst->mName = src->mName;
	dst->mFullPathName = src->mFullPathName;
	dst->mMountPointID = src->mMountPointID;
	dst->mElementRef = src->mElementRef;
    if(src->mKeyFrameController)
        dst->mKeyFrameController = src->mKeyFrameController->clone();
    if(src->mAnimation)
        dst->mAnimation = src->mAnimation->clone();
	dst->mTimeKey = src->mTimeKey;
	dst->mStartKey = src->mStartKey;
	dst->mEndKey = src->mEndKey;
	dst->mKeyFrameNum = src->mKeyFrameNum;
	dst->mSeqNum = src->mSeqNum;
	dst->mURI.setURI(src->mURI.getURI());
	_ElementContentList::iterator it;
	for(it = mElementContentList.begin() ; it != mElementContentList.end() ; it++)
	{
		SE_ElementContent* e = (*it)->clone();
		dst->mElementContentList.push_back(e);
	}
	dst->mMountPointSet = mMountPointSet;
}
struct _ElementPair
{
    SE_Element* oldElement;
    SE_Element* newElement;
};

static SE_Element* findPrevNextElement(SE_Element* e, std::list<_ElementPair>& eList)
{
    std::list<_ElementPair>::itertor it;
    for(it = eList.begin(); it != eList.end() ; it++)
    {
        if(it->oldElement == e)
            return it->newElement;
    }
    return NULL;
} 
SE_Element* SE_Element::clone()
{
    std::list<_ElementPair> elementPair;
	SE_Element* e = new SE_Element;
	clone(this, e);

	if(!mChildren.empty())
	{
		_ElementList::iterator it;
		for(it = mChildren.begin(); it != mChildren.end(); it++)
		{
			SE_Element* child = (*it)->clone();
			_ElementPair p;
            p.oldElement = *it;
            p.newElement = child;
            elementPair.push_back(p);
		}
        std::list<_ElementPair>::iterator itPair;
        for(itPair = elementPair.begin(); itPair != elementPair.end(); itPair++)
        {
            SE_Element* oldElement = it->oldElement;
            SE_Element* newElement = it->newElement;
            if(oldElement->mPrevElement)
            {
                SE_Element* e = findPrevNextElement(oldElement->mPrevElement, elementPair);
                newElement->mPrevElement = e;
            }
            if(oldElement->mNextElement)
            {
                SE_Element* e = findPrevNextElement(oldElement->mNextElement, elementPair);
                newElement->mNextElement = e;
            }
        }
        for(itPair = elementPair.begin() ; itPair != elementPair.end(); itPair++)
        {
            e->mChildren.push_back(itPari->newElement);
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

void SE_Element::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_ElementParamUpdateEvent* event = new SE_ElementParamUpdateEvent;
    event->mElementID = mID;
    event->mParamValueList.add(address, value);
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    elementManager->addEvent(event);
}
void SE_Element::updateSpatial()
{
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Spatial* spatial = sceneManager->findSpatial(mSpatialID);
    SE_Spatial* parentSpatial = sceneManager->findSpatial(mParent->getSpatialID());
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
        sceneManager->addSpatial(parentSpatial, s);
    }
    else if(spatial && parentSpatial)
    {
        sceneManager->removeSpatial(mSpatialID);
        SE_Spatial* s = createSpatial();
        sceneManager->addSpatial(parentSpatial, s);
    }
}
SE_Element* SE_Element::getElement(const SE_StringID& url)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(url.getStr(), "/");
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	SE_Element* e = NULL;
	switch(t)
	{
	case SE_ELEMENT_TABLE:
		e = resourceManager->getElement(url.getStr());
		break;
	case SE_ACTION_TABLE:
		e = new SE_ActionElement(url);
		break;
	case SE_SEQUENCE_TABLE:
		e = new SE_SequenceElement(url);
		break;
	case SE_COLOREFFECT_TABLE:
		e = new SE_ColorEffectControllerElement(url);
		break;
	case SE_IMAGE_TABLE:
		e = new SE_ImageElement(url);
		break;
	default:
		break;
	};
	return e;
}
///////////////
void SE_ElementContent::clone(SE_ElementContent* src, SE_ElementContent* dst)
{
	dst->mContentURI = src->mContentURI;
	dst->mID = src->mID;
}
SE_ElementContent* SE_ElementContent::clone()
{
	return NULL;
}
///////////////////////////////////////
SE_ImageContent::SE_ImageContent(const SE_StringID& imageURI)
{
    setURI(imageURI);
}
SE_ElementContent* SE_ImageContent::clone()
{
	SE_ImageContent* e = new SE_ImageContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_ImageContent::createElement(float mpx, float mpy)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	SE_StringID uri = getURI();
	SE_URI rui(uri.getStr());
	SE_StringID strURL = rui.getURL();
	SE_Util::SplitStringList strList = SE_Util::splitString(strURL.getStr(), "/");
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	SE_Element* rete = NULL;
	if(t == SE_IMAGE_TABLE)
	{
	    SE_ImageElement* imageElement = new SE_ImageElement(getURI());
		imageElement->setMountPoint(mpx, mpy);
		rete = imageElement;
	}
	else if(t == SE_ELEMENT_TABLE)
	{
        SE_TextureElement* textureElement = new SE_TextureElement(uri);
		textureElement->setMountPoint(mpx, mpy);
		rete = textureElement;
	}
	else if(t == SE_COLOREFFECT_TABLE)
	{
		SE_ColorEffectControllerElement* colorEffectControllerElement = new SE_ColorEffectControllerElement(getURI());
		colorEffectControllerElement->setMountPoint(mpx, mpy);
		rete = colorEffectControllerElement;
	}
	return rete;
}
////////////////////////////////////////////////////////
SE_TextureElement::SE_TextureElement(const SE_StringID& uri)
{
    setURI(uri);
    init();
}
SE_TextureElement::~SE_TextureElement()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->removeImageData(mImageDataID);
	SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
	rm->removeRenderTarget(mRenderTargetID);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	elementManager->removeRenderTargetElement(mContentChild);
	if(mContentChild)
		delete mContentChild;
}
void SE_TextureElement::init()
{
	SE_StringID strURL = getURL();
	SE_Util::SplitStringList strList = SE_Util::splitString(strURL.getStr(), "/");
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    mContentChild = resourceManager->getElement(strURL.getStr());
    SE_ImageDataID imageDataID = mContentChild->getFullPathID().getStr();
	SE_ImageData* imageData = resourceManager->getImageData(imageDataID);
	if(!imageData)
	{
		imageData = new SE_ImageData;
		resourceManager->setImageData(imageDataID, imageData);
	}
    mImageData = imageData;
    mImageDataID = imageDataID;
    setPivotX(mContentChild->getPivotX());
    setPivotY(mContentChild->getPivotY());
    mContentChild->setMountPoint(0, 0);
	mContentChild->setPivotX(0);
	mContentChild->setPivotX(0);
    mContentChild->setOwnRenderTargetCamera(true);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	elementManager->addRenderTargetElement(mContentChild);
}
void SE_TextureElement::update(SE_ParamValueList& paramValueList)
{
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    elementManager->removeRenderTargetElement(mContentChild);
    delete mContentChild;
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->removeImageData(mImageDataID);
	SE_RenderTargetManager* rm = SE_Application::getInstance()->getRenderTargetManager();
	rm->removeRenderTarget(mRenderTargetID);
    init();
    spawn();
    measure();
    update(0);
    updateSpatial():
}
void SE_TextureElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}
void SE_TextureElement::setImageData(SE_RectPrimitive* primitive)
{
	primitive->setImageData(mImageData, SE_TEXTURE0, NOT_OWN);
}
void SE_TextureElement::setSurface(SE_Surface* surface)
{
	surface->setProgramDataID(DEFAULT_SHADER);
	surface->setRendererID(DEFAULT_RENDERER);
}
void SE_TextureElement::spawn()
{
	SE_ASSERT(mChildren.size() == 0);
	calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
	if(mContentChild)
		mContentChild->spawn();
	/*
	if(!mChildren.empty())
	{
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			e->spawn();
		}
	}
	*/
}
void SE_TextureElement::update(unsigned int key)
{
	if(mContentChild)
		mContentChild->update(key);
}
void SE_TextureElement::setImage(const SE_ImageDataID& id, SE_ImageData* imageData)
{
	mImageDataID = id;
	mImageData = imageData;
}
void SE_TextureElement::measure()
{
	if(!mContentChild)
	{
		return;
	}
    mContentChild->measure();
	mWidth = mContentChild->getWidth();
	mHeight = mContentChild->getHeight();
	SE_TextureTarget* textureTarget = new SE_TextureTarget(mImageData);
	SE_RenderTargetManager* renderTargetManager = SE_Application::getInstance()->getRenderTargetManager();
    mRenderTargetID = renderTargetManager->addRenderTarget(textureTarget);
	textureTarget->setBackground(SE_Vector4f(0, 0, 0, 0));
	mContentChild->setRenderTarget(mRenderTargetID);
	float ratio = mHeight / mWidth;
	float angle = 2 * SE_RadianToAngle(atanf(mWidth / 20.0f));
    SE_Camera* camera = new SE_Camera;
	float left = mContentChild->getDeltaLeft() + mWidth / 2;
	float top = mContentChild->getDeltaTop() + mHeight / 2;
	SE_Vector3f v(left , top , 10);
	camera->setLocation(v);
	camera->create(v, SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), SE_Vector3f(0, 0, 1), angle, ratio, 1, 50);
	camera->setViewport(0, 0, mWidth, mHeight);
	mImageData->setWidth(mWidth);
	mImageData->setHeight(mHeight);
	mImageData->setPixelFormat(SE_ImageData::RGBA);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_CameraID cameraID = SE_ID::createCameraID();
	cameraManager->setCamera(cameraID, camera);
	textureTarget->setWidth(mWidth);
	textureTarget->setHeight(mHeight);
	textureTarget->setCamera(cameraID);
	textureTarget->create();
}
SE_Spatial* SE_TextureElement::createSpatial()
{
    //SE_CommonNode* node = new SE_CommonNode;
    SE_Spatial* spatial = createSpatialByImage();
	return spatial;
	//if(spatial)
	//    node->addChild(spatial);
	//return node;

	/*
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
	*/
}
/////////
SE_ActionContent::SE_ActionContent(const SE_StringID& actionURI)
{
	setURI(actionURI);
}
SE_ElementContent* SE_ActionContent::clone()
{
	SE_ActionContent* e = new SE_ActionContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_ActionContent::createElement(float mpx, float mpy)
{
	SE_ActionElement* e = new SE_ActionElement(getURI());
	e->setMountPoint(mpx, mpy);
	return e;
}
SE_StateTableContent::SE_StateTableContent(const SE_StringID& stateTableURI)
{
	setURI(stateTableURI);
}
SE_ElementContent* SE_StateTableContent::clone()
{
	SE_StateTableContent* e = new SE_StateTableContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_StateTableContent::createElement(float mpx, float mpy)
{
	SE_StateTableElement* e = new SE_StateTableElement(getURI());
	e->setMountPoint(mpx, mpy);
	return e;
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
SE_ImageElement::SE_ImageElement(const SE_StringID& uri)
{
	setURI(uri);
	mImageUnits[0].imageUnit = &mBaseColor;
	mImageUnits[1].imageUnit = &mRChannel;
    mImageUnits[2].imageUnit = &mGChannel;
	mImageUnits[3].imageUnit = &mBChannel;
    mImageUnits[4].imageUnit = &mAChannel;
    initImage();
}
SE_ImageElement::~SE_ImageElement()
{

}
void SE_ImageElement::initImage()
{
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ExtractImageStr imageStr = SE_Util::stringToExtractImage(url.getStr());
	mBaseColor = resourceManager->getImageUnit(imageStr.base.c_str());
	mRChannel =  resourceManager->getImageUnit(imageStr.red.c_str());
	mGChannel =  resourceManager->getImageUnit(imageStr.green.c_str());
	mBChannel =  resourceManager->getImageUnit(imageStr.blue.c_str());
	mAChannel = resourceManager->getImageUnit(imageStr.alpha.c_str());
}
void SE_ImageElement::update(SE_ParamValueList& paramValueList)
{
    initImage();
    spawn();
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Spatial* spatial = sceneManager->findSpatial(mSpatialID);
    SE_Spatial* parentSpatial = sceneManager->findSpatial(mParent->getSpatialID());
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
        sceneManager->addSpatial(parentSpatial, s);
    }
    else if(spatial && parentSpatial)
    {
        sceneManager->removeSpatial(mSpatialID);
        SE_Spatial* s = createSpatial();
        sceneManager->addSpatial(parentSpatial, s);
    }
}
void SE_ImageElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}

bool SE_ImageElement::isValid()
{
	int count = 0;
	for(int i = 0 ; i < IMG_SIZE ; i++)
	{
		_ImageUnitData* iuData = &mImageUnits[i];
		if(iuData->imageUnit->imageDataID.isValid())
			count++;
	}
	return count > 0;
}
void SE_ImageElement::setImageData(SE_RectPrimitive* primitive)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	int j = 0;
    for(int i = 0 ; i < IMG_SIZE ; i++)
	{
		SE_ImageUnit* imageUnit = mImageUnits[i].imageUnit;
		if(imageUnit->isValid())
		{
			SE_ImageData* imageData = resourceManager->getImageData(imageUnit->imageDataID.getStr());
		    if(!imageData)
		    {
			    imageData = resourceManager->loadImage(imageUnit->imageDataID.getStr());
		    } 
            SE_ImageDataPortion dp;
		    dp.setX(imageUnit->imageRect.x);
		    dp.setY(imageUnit->imageRect.y);
		    dp.setWidth(imageUnit->imageRect.width);
		    dp.setHeight(imageUnit->imageRect.height);
		    primitive->setImageData(imageData, (SE_TEXUNIT_TYPE)j, NOT_OWN, dp);
			j++;
			mImageUnits[i].valid = 1;
		}
	}
}
static int getColorIndex(const std::string c)
{
	if(c == "r")
		return 0;
	else if(c == "g")
		return 1;
	else if(c == "b")
		return 2;
	else if(c == "a")
		return 3;
	return 0;
}
void SE_ImageElement::setSurface(SE_Surface* surface)
{
	static const int DEFAULT_COLOR = 0;
    static const int REPLACE_R = 1;
    static const int REPLACE_G = 2;
    static const int REPLACE_B = 3;
    static const int REPLACE_A = 4;

    static const int REPLACE_RG = 5;
    static const int REPLACE_RB = 6;
    static const int REPLACE_RA = 7;
    static const int REPLACE_GB = 8;
    static const int REPLACE_GA = 9;
    static const int REPLACE_BA = 10;

    static const int REPLACE_RGB = 11;
    static const int REPLACE_RGA = 12;
    static const int REPLACE_GBA = 13;
    static const int REPLACE_RBA = 14;
    static const int REPLACE_RGBA = 15;
    static const int NO_REPLACE = 16;
	int pattern[] = {0, REPLACE_A, REPLACE_B, REPLACE_BA, REPLACE_G, REPLACE_GA, 
	                 REPLACE_GB, REPLACE_GBA, REPLACE_R, REPLACE_RA, REPLACE_RB, REPLACE_RBA,
	                 REPLACE_RG, REPLACE_RGA, REPLACE_RGB, REPLACE_RGBA, NO_REPLACE};
	int index = 0;
	SE_ColorExtractShaderProperty* sp = new SE_ColorExtractShaderProperty;
	for(int i = 1 ; i < 5 ; i++)
	{
        index |= (mImageUnits[i].valid << (4 - i));
		if(mImageUnits[i].valid)
		{
			std::string ext = mImageUnits[i].imageUnit->ext.getStr();
			int c = getColorIndex(ext);
			sp->setColorChannelIndex(i - 1, c);
		}
	}
	int op = pattern[index];
	if(mBaseColor.isValid() && op == 0)
	{
		op = NO_REPLACE;
	}
	sp->setColorOperationMode(op);
	surface->setShaderProperty(sp);
    surface->setProgramDataID(COLOREXTRACT_SHADER);
	surface->setRendererID(COLOREXTRACT_RENDERER);
}
void SE_ImageElement::update(unsigned int)
{}
void SE_ImageElement::spawn()
{
	SE_ImageUnit iu = mBaseColor;
	float pivotx, pivoty, width, height;
	if(iu.imageDataID.isValid())
	{
		width = iu.imageRect.width;
		height = iu.imageRect.height;
		pivotx = iu.imageRect.pivotx;
		pivoty = iu.imageRect.pivoty;
	}
	else
	{
		iu = mRChannel;
		width = iu.imageRect.width;
		height = iu.imageRect.height;
		pivotx = iu.imageRect.pivotx;
		pivoty = iu.imageRect.pivoty;
	}
	calculateRect(pivotx, pivoty, width, height);
}
void SE_ImageElement::measure()
{
}
SE_Spatial* SE_ImageElement::createSpatial()
{
	if(isValid())
	{
	    return createSpatialByImage();
	}
	else
		return NULL;

}

/////////////////////////
SE_ActionElement::SE_ActionElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_StringID url = getURL();
	mAction = resourceManager->getAction(url.getStr());
}
void SE_ActionElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}
void SE_ActionElement::update(SE_ParamValueList& paramValueList)
{
    if(mAction)
        delete mAction;
    SE_StringID url = getURL();
    mAction = resourceManager->getAction(url.getStr());
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Spatial* s = sceneManager->removeSpatial(mSpatialID);
    delete s;
    spawn();
    measure();
    update(0);
    updateSpatial();

}
void SE_ActionElement::spawn()
{
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
	node->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
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
SE_SequenceElement::SE_SequenceElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mSequence = resourceManager->getSequence(url.getStr());
	mCurrentElement = NULL;
}
void SE_SequenceElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}
void SE_SequenceElement::update(SE_ParamValueList& paramValueList)
{
    SE_StringID url = getURL();
    if(mSequence)
        delete mSequence;
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mSequence = resourceManager->getSequence(url.getStr());
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Spatial* s = sceneManager->removeSpatial(mSpatialID);
    delete s;
    clearChildren();
    spawn();
    measure();
    update(0);
    updateSpatial();
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
	if(!mSequence)
		return;
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
		SE_ImageElement* e = new SE_ImageElement(f.imageref);
		e->setMountPointRef(f.mpref);
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
SE_StateTableElement::SE_StateTableElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mStateTable = resourceManager->getStateMachine(url.getStr());
}
void SE_StateTableElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}
void SE_StateTableElement::update(SE_ParamValueList& paramValueList)
{

}
void SE_StateTableElement::update(unsigned int key)
{}
void SE_StateTableElement::spawn()
{
    calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
	SE_State* currState = mStateTable->getCurrentState();
	SE_ASSERT(currState != NULL);
	if(!currState)
		return;
	SE_StringID strURI = currState->getDefaultValue();
	SE_URI uri(strURI.getStr());
	SE_StringID url = uri.getURL();
	SE_Element* e = getElement(url);
	this->addChild(e);
	e->spawn();
}
void SE_StateTableElement::measure()
{
	SE_Element::measure();
}
SE_Spatial* SE_StateTableElement::createSpatial()
{
	return SE_Element::createSpatial();
}
///////////////////
SE_ColorEffectControllerElement::SE_ColorEffectControllerElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mColorEffectController = resourceManager->getColorEffectController(url.getStr());
	mID = mColorEffectController->getID().getStr();
	mCurrentElement = NULL;
}
void SE_ColorEffectControllerElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}

void SE_ColorEffectControllerElement::update(unsigned int key)
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
		/*
		SE_Spatial* spatial = first->createSpatial();
		SE_Spatial* parentSpatial = sceneManager->find(getSpatialID());
		sceneManager->addSpatial(parentSpatial, spatial);
		spatial->updateRenderState();
		spatial->updateWorldLayer();
		spatial->updateWorldTransform();
		*/
		mCurrentElement = first;
	}
}
SE_Spatial* SE_ColorEffectControllerElement::createSpatial()
{
	if(mCurrentElement)
	{
	    SE_Spatial* node = createNode();
        SE_Spatial* spatial = mCurrentElement->createSpatial();
		node->addChild(spatial);
		return node;
	}
	else
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
void SE_ColorEffectControllerElement::measure()
{
	SE_Element::measure();
}
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
void SE_ColorEffectElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_Element::update(address, value);
}

void SE_ColorEffectElement::update(unsigned int key)
{}
SE_ImageElement* SE_ColorEffectElement::createImageElement(const SE_StringID& textureURL, SE_ImageData*& imageData)
{
    SE_ImageElement* imageElement = new SE_ImageElement(textureURL);
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
bool SE_ColorEffectElement::isTextureEnd(_ElementList::const_iterator textureIt[4], SE_Element* texture[4])
{
	bool ret = false;
	for(int i = 0 ; i < MARK_NUM ; i++)
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
		_ElementList::const_iterator backgroundIt = background->getChildren().begin();
		_ElementList::const_iterator channelIt = channel->getChildren().begin();
		_ElementList::const_iterator textureIt[MARK_NUM];
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			if(texture[i])
				textureIt[i] = texture[i]->getChildren().begin();
		}
		while(backgroundIt != background->getChildren().end() &&
			channelIt != channel->getChildren().end() && !isTextureEnd(textureIt, texture))
		{
			SE_Element* bc = *backgroundIt;
			SE_Element* cc = *channelIt;
			SE_Element* tc[MARK_NUM] = {NULL, NULL, NULL, NULL};
			for(int i = 0 ; i < MARK_NUM ; i++)
			{
				if(texture[i])
					tc[i] = *textureIt[i];
			}
			SE_Element* child = mergeElement(bc, cc, tc);
			element->addChild(child);
			backgroundIt++;
			channelIt++;
			for(int i = 0 ; i < MARK_NUM ; i++)
			{
				if(texture[i])
					textureIt[i]++;
			}
		}
		SE_ASSERT(backgroundIt == background->getChildren().end() && channelIt == channel->getChildren().end());
		for(int i = 0 ; i < MARK_NUM ; i++)
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
        SE_StringID backgroundImageURI = background->getURI();
		SE_StringID channelImageURI = channel->getURI();
		SE_StringID textureURI[MARK_NUM];
		for(int i = 0 ; i < MARK_NUM ; i++)
		{
			if(texture[i])
				textureURI[i] = texture[i]->getURI();
		}
		SE_ColorEffectElement* retElement = new SE_ColorEffectElement;
		clone(background, retElement);
		retElement->setBackgroundValue(backgroundImageURI);
		retElement->setChannelValue(channelImageURI);
		retElement->setBackgroundAlphaValue(mBackgroundAlphaValue);
		retElement->setBackgroundAlphaAddress(mBackgroundAlphaAddress);
		for(int i = 0 ; i < MARK_NUM ; i++)
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
    SE_ColorEffectShaderProperty* cfp = new SE_ColorEffectShaderProperty;
	cfp->setBackgroundTexture(SE_TEXTURE0);
	cfp->setChannelTexture(SE_TEXTURE1);
	cfp->setBackgroundAlpha(mBackgroundAlphaValue);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        _TextureMark& tm = mTextureMark[i];
		if(tm.valid)
		{
			cfp->setHasMark(i, true);
		}
		else
			cfp->setHasMark(i, false);
		if(tm.mTextureValue.isValid())
		{
			cfp->setHasTex(i, true);
		}
		else
		{
			cfp->setHasTex(i, false);
		}
		cfp->setMarkAlpha(i, tm.mColorAlphaValue);
		SE_Vector3i color;
		color.x = tm.mColorValue.data[0].value;
		color.y = tm.mColorValue.data[1].value;
		color.z = tm.mColorValue.data[2].value;
		cfp->setMarkColor(i, SE_Vector3f(color.x /255.0, color.y / 255.0, color.z / 255.0));
		cfp->setMarkFunction(i, tm.mFnValue);
	}
	surface->setShaderProperty(cfp);
	surface->setProgramDataID(COLOREFFECT_SHADER);
	surface->setRendererID(COLOREFFECT_RENDERER);
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
        return createSpatialByImage();
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
int SE_ColorEffectElement::uriToInt(const SE_StringID& uri)
{
	SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	SE_URI intURI(uri.getStr());
	SE_StringID intURL = intURI.getURL();
	int ret = -1;
	if(SE_Util::isDigit(intURL.getStr()))
	{
		ret = atoi(intURL.getStr());
	}
	else
	{
        bool ok = false;
		int tmp = paramManager->getInt(intURL.getStr(), ok);
		if(ok)
			ret = tmp;
	}
	return ret;
}
void SE_ColorEffectElement::calculateValue()
{
	SE_URI background(mBackgroundAddress.getStr());
	mBackgroundValue = background.getURL();
	SE_URI channel(mChannelAddress.getStr());
	mChannelValue = channel.getURL();
    mBackgroundAlphaValue = uriToInt(mBackgroundAlphaAddress);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
        _TextureMark& tm = mTextureMark[i];
		if(tm.valid)
		{
		    SE_URI textureURI(tm.mTextureAddress.getStr());
		    tm.mTextureValue = textureURI.getURL();
		    tm.mFnValue = uriToInt(tm.mFnAddress);
		    tm.mColorAlphaValue = uriToInt(tm.mColorAlphaAddress);
		    SE_URI colorURI(tm.mColorAddress.getStr());
		    SE_StringID colorURL = colorURI.getURL();
		    tm.mColorValue = SE_Util::stringToSignColor(colorURL.getStr());
		    colorURI.setURI(tm.mColor2Address);
		    colorURL = colorURI.getURL();
		    tm.mColor2Value = SE_Util::stringToSignColor(colorURL.getStr());
		    tm.mTextureFnValue = uriToInt(tm.mTextureFnAddress);
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
		if(tm.valid)
		{
		    SE_ExtractImageStr textureExtractImage = SE_Util::stringToExtractImage(tm.mTextureValue.getStr());
		    tm.mTextureArity = textureExtractImage.getImageNum();
		}
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
