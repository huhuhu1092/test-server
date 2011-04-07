#include "SE_ActionElement.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_Spatial.h"
#include "SE_Action.h"
#include "SE_CommonNode.h"
#include "SE_ElementKeyFrameAnimation.h"
#include <vector>
SE_ActionElement::SE_ActionElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_StringID url = getURL();
	mAction = resourceManager->getAction(url.getStr());
    if(mAction)
        setMountPoint(mAction->getMountPoint());
}
SE_Element* SE_ActionElement::clone()
{
	SE_ActionElement* element = new SE_ActionElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
void SE_ActionElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}
void SE_ActionElement::update(SE_ParamValueList& paramValueList)
{
    SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    mAction = resourceManager->getAction(url.getStr());
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* s = spatialManager->remove(getID());
    spawn();
    layout();
    update(0);
    updateSpatial();
	spatialManager->release(s);
}
void SE_ActionElement::layout()
{
    SE_2DNodeElement::layout(); 
}
void SE_ActionElement::spawn()
{
	if(!mAction)
		return;
	SE_ElementKeyFrameAnimation* anim = new SE_ElementKeyFrameAnimation;
	anim->setElement(getID());
	int frameRate = SE_Application::getInstance()->getFrameRate();
	SE_TimeKey diff = mAction->getEndKey();
	SE_TimeMS duration = diff.toInt() * frameRate;
	anim->setDuration(duration);
	this->setAnimation(anim);
}
SE_Spatial* SE_ActionElement::createSpatial()
{
	if(!mAction)
		return NULL;
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* node = createNode(mLeft, mTop);
	std::vector<SE_Element*> children = getChildren();
	std::vector<SE_Element*>::iterator it;
	for(it = children.begin() ; it != children.end() ; it++)
	{
		SE_Element* e = *it;
		SE_Spatial* spatial = e->createSpatial();
		if(spatial)
		    spatialManager->add(node, spatial);
	}
	return node;
}
void SE_ActionElement::update(const SE_TimeKey& key)
{
    clearChildren();
    std::list<SE_Element*> childElement = mAction->createElement(key);
    SE_ElementManager* elementManager = SE_GET_ELEMENTMANAGER();
    std::list<SE_Element*>::iterator it;
    for(it = childElement.begin() ; it != childElement.end() ; it++)
    {
        if(*it)
        {
            elementManager->add(this->getID(), *it, true);
        }
    }
	std::vector<SE_Element*> children = getChildren();
	std::vector<SE_Element*>::iterator itChild;
	for(itChild = children.begin() ; itChild != children.end() ; itChild++)
	{
		SE_Element* e = *itChild;
		e->spawn();
        e->update(key - e->getTimeKey());
	}
    /*
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
    */
}
