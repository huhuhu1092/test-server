#include "SE_Action.h"
#include "SE_2DElement.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_ElementManager.h"
#include "SE_Sequence.h"
#include "SE_ColorEffectController.h"
#include "SE_ElementSchema.h"
#include <algorithm>

struct _EqualTest
{
    bool operator()(const SE_ActionUnit* data)
    {
        if(data->getID() == id)
            return true;
        else
            return false;
    }
    SE_StringID id;
};
//////////////////////////////////
SE_Element* SE_ImageAnimationObject::createElement()
{
    SE_ImageElement* imageElement = new SE_ImageElement(mImageRef);
	imageElement->setMountPointRef(getMountPointRef());
	return imageElement;
}
SE_Element* SE_SequenceAnimationObject::createElement()
{
    SE_SequenceElement* element = new SE_SequenceElement(mSequenceFrameRef);
	element->setMountPointRef(getMountPointRef());
    return element;
	
}
std::vector<SE_TimeKey> SE_SequenceAnimationObject::getKeys() const
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_Sequence* sequence = resourceManager->getSequence(mSequenceFrameRef.getStr());
	if(!sequence)
        return std::vector<SE_TimeKey>();
    return sequence->getKeys();
}
/////////
SE_Element* SE_ColorEffectAnimationObject::createElement()
{
	SE_ColorEffectControllerElement* element = new SE_ColorEffectControllerElement(mColorEffectRef);
	element->setMountPointRef(this->getMountPointRef());
	return element;
}
SE_Element* SE_TextureAnimationObject::createElement()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_URI uri(mTextureRef.getStr());
	SE_StringID strURL = uri.getURL();
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strURL.getStr());
	switch(t)
	{
	case SE_ELEMENT_TABLE:
		{
			//resourceManager->loadElement(strURL.getStr());
			SE_ElementSchema* elementSchema = resourceManager->getElementSchema(strURL.getStr());
            SE_2DNodeElement* element = (SE_2DNodeElement*)elementSchema->createElement();
			element->setMountPointRef(getMountPointRef());
			return element;
		}
	    break;
	case SE_ACTION_TABLE:
		{
			//resourceManager->loadAction(mTextureRef.getStr());
			SE_ActionElement* actionElement = new SE_ActionElement(mTextureRef);//(SE_ActionElement*)resourceManager->getElement(mTextureRef.getStr());
			actionElement->setMountPointRef(getMountPointRef());
			return actionElement;
		}
		break;
	default:
		return NULL;
	}
}
SE_Element* SE_DeleteAction::createElement()
{
	return NULL;
}
SE_Element* SE_MusicObjectAction::createElement()
{
	return NULL;
}
//////////////////////////////////
SE_Action::SE_Action()
{
	mRenderMode = RENDER_TO_BUFFER;
}
SE_Action::~SE_Action()
{}
bool SE_Action::compareActionLayer(_ActionLayer* first, _ActionLayer* second)
{
	if(first->layer < second->layer)
		return true;
	else
		return false;
}
bool SE_Action::compareEndKey(const _EndKey& first, const _EndKey& second)
{
    if(first.layer < second.layer)
        return true;
    else
        return false;
 
}
SE_Action::_EndKey SE_Action::getAllLayerEndKey()
{
    if(mEndKeyList.empty())
        return _EndKey();
    _EndKeyEqual ekEqual;
	ekEqual.ek.layer = SE_Layer(0);
    ekEqual.justCompareLayer = true;
    _EndKeyList::iterator it = find_if(mEndKeyList.begin(), mEndKeyList.end(), ekEqual);
    if(it != mEndKeyList.end())
        return *it;
    else
        return _EndKey();
}
void SE_Action::sort()
{
	mActionLayerList.sort(&SE_Action::compareActionLayer);
	_ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* al = *it;
		std::vector<SE_TimeKey> keys = al->sequences.getKeys();
		std::vector<SE_TimeKey>::iterator it;
		it = min_element(keys.begin(), keys.end());
		if(it != keys.end())
			al->startkey = *it;
		it = max_element(keys.begin(), keys.end());
		if(it != keys.end())
		    al->endkey = *it;
		if(al->startkey == al->endkey)
		{
			SE_KeyFrame<SE_ActionUnit*>* kf = al->sequences.getKeyFrame(al->endkey);;
			SE_ActionUnit* au = kf->data;
			std::vector<SE_TimeKey> keys = au->getKeys();
			std::sort(keys.begin(), keys.end());
			if(!keys.empty())
			{
				al->endkey += keys[keys.size() - 1];
			}
		}
	}
    mEndKeyList.sort(&SE_Action::compareEndKey);
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* al = *it;
        SE_Layer layer = al->layer;
        _EndKeyEqual eqEqual;
        eqEqual.justCompareLayer = true;
        eqEqual.ek.layer = layer;
        _EndKeyList::iterator itKey = find_if(mEndKeyList.begin(), mEndKeyList.end(), eqEqual);
        if(itKey != mEndKeyList.end())
        {
            al->endkey = itKey->key;
        }
        else
        {
            _EndKey ek = getAllLayerEndKey();
			if(ek.key != 0)
                al->endkey = ek.key;
        }
    }    
}
void SE_Action::addKeyFrame(SE_KeyFrame<SE_ActionUnit*>* keyframe)
{
	_ActionLayerList::iterator it;
	_ActionLayer* currLayer = NULL;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* al = *it;
		if(al->layer == keyframe->data->getLayer())
		{
			currLayer = al;
			break;
		}
	}
	if(currLayer)
	{
		currLayer->sequences.addKeyFrame(keyframe);
	}
	else
	{
        _ActionLayer* al = new _ActionLayer;
		al->layer = keyframe->data->getLayer();
		al->sequences.addKeyFrame(keyframe);
		mActionLayerList.push_back(al);
	}
}
void SE_Action::addActionUnit(unsigned int key, SE_ActionUnit* au)
{
	SE_KeyFrame<SE_ActionUnit*>* keyframe = new SE_KeyFrame<SE_ActionUnit*>;
	keyframe->data = au;
	keyframe->key = key;
	addKeyFrame(keyframe);
}
template <>
struct SE_KeyFrameCompare<SE_ActionUnit*>
{
    bool operator()(const SE_KeyFrame<SE_ActionUnit*>* keyframe)
    {
        if(keyframe->data->getID() == id)
            return true;
        else
            return false;
    }
    SE_StringID id;
};
void SE_Action::removeActionUnit(const SE_StringID& auID)
{
    _ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
	    _ActionLayer* currLayer = *it;
        SE_KeyFrameCompare<SE_ActionUnit*> eq;
        eq.id = auID;
        currLayer->sequences.remove_if(eq);
    }
}
SE_ActionUnit* SE_Action::getActionUnit(const SE_StringID& auID)
{
    _ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
	    _ActionLayer* currLayer = *it;
        SE_KeyFrameCompare<SE_ActionUnit*> eq;
        eq.id = auID;
        SE_KeyFrame<SE_ActionUnit*>* keyframe = currLayer->sequences.find(eq);
        if(keyframe != NULL)
        {
            return keyframe->data;
        }
    }    
    return NULL;
}

void SE_Action::addEndKey(unsigned int key, const SE_Layer& layer)
{
    _EndKey ek;
    ek.key = key;
    ek.layer = layer;
    mEndKeyList.push_back(ek);
}
void SE_Action::removeEndKey(unsigned int key, const SE_Layer& layer)
{
    _EndKeyEqual ekEqual;
    ekEqual.ek.key = key;
    ekEqual.ek.layer = layer;
    ekEqual.justCompareLayer = false;
    mEndKeyList.remove_if(ekEqual);
}
void SE_Action::createElement(SE_ActionElement* parent)
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	parent->clearMountPoint();
	std::vector<SE_MountPoint> mountPoint = mMountPointSet.getMountPoint();
	for(int i = 0 ; i < mountPoint.size() ; i++)
	{
		parent->addMountPoint(mountPoint[i]);
	}
	_ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* actionLayer = *it;
		std::vector<SE_TimeKey> keys = actionLayer->sequences.getKeys();
		SE_Element* prev = NULL;
		SE_Element* next = NULL;
		SE_Element* first = NULL;
		for(int i = 0 ; i < keys.size() ; i++)
		{
			SE_KeyFrame<SE_ActionUnit*>* kf = actionLayer->sequences.getKeyFrame(keys[i]);
			SE_Element* e = kf->data->createElement();
			e->setLocalLayer(actionLayer->layer);
			e->setTimeKey(keys[i]);
			e->setStartKey(actionLayer->startkey);
			e->setEndKey(actionLayer->endkey);
			elementManager->addElement(parent, e);
			//e->setParent(parent);
			if(prev)
			{
				prev->setNext(e);
			}
			e->setPrev(prev);
			e->setNext(next);
			prev = e;
            if(i == 0)
				first = e;
		}
		parent->addHeadElement(first);
	}
}
