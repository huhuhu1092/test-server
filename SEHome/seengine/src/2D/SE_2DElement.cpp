#include "SE_2DElement.h"
#include "SE_Utils.h"
#include "SE_ActionElement.h"
#include "SE_ImageElement.h"
#include "SE_TextureElement.h"
#include "SE_StateTableElement.h"
#include "SE_SequenceElement.h"
#include "SE_ColorEffectControllerElement.h"
#include "SE_URI.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ElementSchema.h"
//////////////////////////////
/*
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

*/
static SE_2DNodeElement* getElement(const SE_StringID& uri)
{
	SE_URI strURI(uri.getStr());
    SE_StringID url = strURI.getURL();
	SE_Util::SplitStringList strList = SE_Util::splitString(url.getStr(), "/");
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	SE_2DNodeElement* e = NULL;
	SE_ElementSchema* es = NULL;
	switch(t)
	{
	case SE_ELEMENT_TABLE:
		es = resourceManager->getElementSchema(url.getStr());
		e = (SE_2DNodeElement*)es->createElement();
		break;
	case SE_ACTION_TABLE:
		e = new SE_ActionElement(uri);
		break;
	case SE_SEQUENCE_TABLE:
		e = new SE_SequenceElement(uri);
		break;
	case SE_COLOREFFECT_TABLE:
		e = new SE_ColorEffectControllerElement(uri);
		break;
	case SE_IMAGE_TABLE:
		e = new SE_ImageElement(uri);
		break;
	default:
		break;
	};
	return e;
}
SE_2DNodeElement* SE_GetElement(const SE_StringID& uri)
{
	return getElement(uri);
}

