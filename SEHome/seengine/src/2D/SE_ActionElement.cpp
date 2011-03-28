#include "SE_ActionElement.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialManager.h"
#include "SE_Spatial.h"
#include "SE_Action.h"
#include <vector>
SE_ActionElement::SE_ActionElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_StringID url = getURL();
	mAction = resourceManager->getAction(url.getStr());
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
void SE_ActionElement::spawn()
{
	if(!mAction)
		return;
	mAction->sort();
	calculateRect(mAction->getPivotX(), mAction->getPivotY(), 0, 0);
	mAction->createElement(this);
	std::vector<SE_Element*> children = getChildren();
	std::vector<SE_Element*>::iterator it;
	for(it = children.begin() ; it != children.end() ; it++)
	{
		SE_Element* e = *it;
		e->spawn();
	}
}
SE_Spatial* SE_ActionElement::createSpatial()
{
	if(!mAction)
		return NULL;
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_CommonNode* node = new SE_CommonNode;
	node->setLocalTranslate(SE_Vector3f(mLeft, mTop, 0));
    node->setLocalLayer(mLocalLayer);
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
