#include "SE_SequenceElement.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_SpatialManager.h"
#include "SE_ImageElement.h"
#include "SE_ElementManager.h"
#include "SE_ElementKeyFrameAnimation.h"
#include "SE_CommonNode.h"
SE_SequenceElement::SE_SequenceElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mSequence = resourceManager->getSequence(url.getStr());
    if(mSequence)
    {
        std::vector<SE_MountPoint> mountPoints = mSequence->getMountPoint();
	    for(int i = 0 ; i < mountPoints.size(); i++)
	    {
		    mMountPointSet.addMountPoint(mountPoints[i]);
	    }
    }
	//mCurrentElement = NULL;
}
void SE_SequenceElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}
SE_Element* SE_SequenceElement::clone()
{
	SE_SequenceElement* element = new SE_SequenceElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
void SE_SequenceElement::update(SE_ParamValueList& paramValueList)
{
    SE_StringID url = getURL();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mSequence = resourceManager->getSequence(url.getStr());
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* s = spatialManager->remove(getID());
	spatialManager->release(s);
    clearChildren();
    spawn();
    update(0);
    layout();
    updateSpatial();
}
void SE_SequenceElement::layout()
{
    SE_2DNodeElement::layout();
}
int SE_SequenceElement::getKeyFrameNum()
{
	if(!mSequence)
		return 0;
    std::vector<SE_TimeKey> keys = mSequence->getKeys();
    return keys.size();
}
void SE_SequenceElement::spawn()
{
	if(!mSequence)
		return;
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    std::list<SE_Element*> childElement = mSequence->createElement(SE_TimeKey(0));
    std::list<SE_Element*>::iterator it;
    for(it = childElement.begin() ; it != childElement.end() ; it++)
    {
        SE_Element* e = *it;
        elementManager->add(this->getID(), e, true);
        e->spawn();
    }
    /*
	std::vector<SE_TimeKey> keys = mSequence->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_Sequence::_Frame f = mSequence->getFrame(keys[i]);
		SE_ImageElement* e = new SE_ImageElement(f.imageref);
		e->setMountPointRef(f.mpref);
		e->setTimeKey(keys[i]);
		elementManager->add(this->getID(), e, true);
		e->spawn();
	}
    */

}
SE_Spatial* SE_SequenceElement::createSpatial()
{
	if(!mSequence)
		return NULL;
	return SE_2DNodeElement::createSpatial();
}
void SE_SequenceElement::update(const SE_TimeKey& key)
{
    clearChildren();
    std::list<SE_Element*> childElement = mSequence->createElement(key);
    std::list<SE_Element*>::iterator it;
    SE_ElementManager* elementManager = SE_GET_ELEMENTMANAGER();
    for(it = childElement.begin() ; it != childElement.end() ; it++)
    {
        SE_Element* e = *it;
        elementManager->add(this->getID(), e, true);
        e->spawn();
        e->update(key - e->getTimeKey());
    }
    /*
	std::vector<SE_Element*> children = getChildren();
	if(children.empty())
		return;
	std::vector<SE_Element*>::iterator it;
	SE_Element* first = NULL;
	SE_Element* second = NULL;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	std::vector<SE_Element*>::iterator currIt = children.end();
	for(it = children.begin() ; it != children.end() ; it++)
	{
		SE_Element* e = *it;
		if(e->getTimeKey() >= key)
		{
            currIt = it;
			break;
		}
	}
	if(currIt == children.end())
	{
		currIt--;
		first = *currIt;
		second = NULL;
	}
	else
	{
		if(currIt == children.begin())
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
			mCurrentElement->dismiss();
		}
		SE_Spatial* spatial = first->createSpatial();
		SE_Spatial* parentSpatial = spatialManager->get(getID());
		spatialManager->add(parentSpatial->getID(), spatial, true);
		spatial->updateRenderState();
		spatial->updateWorldLayer();
		spatial->updateWorldTransform();
		mCurrentElement = (SE_2DNodeElement*)first;
	}}
    */
}
