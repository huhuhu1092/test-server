#include "SE_Action.h"
#include "SE_Element.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_Sequence.h"
#include "SE_ColorEffectController.h"
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
    SE_ImageElement* imageElement = new SE_ImageElement;
	imageElement->setImage(mImageRef);
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageUnit imageUnit = resourceManager->getImageUnit(mImageRef.getStr());
	imageElement->setPivotX(imageUnit.imageRect.pivotx);
	imageElement->setPivotY(imageUnit.imageRect.pivoty);
	return imageElement;
}
SE_Element* SE_SequenceAnimationObject::createElement()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_Sequence* sequence = resourceManager->getSequence(mSequenceFrameRef.getStr());
	if(!sequence)
		return NULL;
    SE_SequenceElement* element = new SE_SequenceElement(sequence);
	element->setPivotX(sequence->getPivotX());
    element->setPivotY(sequence->getPivotY());
	element->setMountPointRef(getMountPointRef());
    return element;
	
}
std::vector<unsigned int> SE_SequenceAnimationObject::getKeys()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_Sequence* sequence = resourceManager->getSequence(mSequenceFrameRef.getStr());
	if(!sequence)
        return std::vector<unsigned int>();
    return sequence->getKeys();
}
/////////
SE_Element* SE_ColorEffectAnimationObject::createElement()
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();	
	SE_ColorEffectController* colorEffectController = resourceManager->getColorEffectController(mColorEffectRef.getStr());
	if(!colorEffectController)
		return NULL;
	SE_ColorEffectElement* element = new SE_ColorEffectElement(colorEffectController, mColorEffectInput);
	element->setPivotX(colorEffectController->getPivotX());
	element->setPivotY(colorEffectController->getPivotY());
	element->setMountPointRef(this->getMountPointRef());
	return element;
}
SE_Element* SE_TextureAnimationObject::createElement()
{
	return NULL;
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
		std::vector<unsigned int> keys = al->sequences.getKeys();
		std::vector<unsigned int>::iterator it;
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
			std::vector<unsigned int> keys = au->getKeys();
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
		std::vector<unsigned int> keys = actionLayer->sequences.getKeys();
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
			parent->addChild(e);
			e->setParent(parent);
			e->spawn();
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
		/*
		if(actionLayer->startkey == 0)
		{
			SE_KeyFrame<SE_ActionUnit*>* kf = actionLayer->sequences.getKeyFrame(0);
			SE_Element* element = kf->data->createElement();
			element->setStartKey(actionLayer->startkey);
			element->setEndKey(actionLayer->endkey);
            element->setActionLayer(actionLayer);
            element->setLocalLayer(kf->data->getLayer());
			element->setParent(parent);
			parent->addChild(element);
		}
		else
		{
            SE_NullElement* e = new SE_NullElement;
			e->setActionLayer(actionLayer);
			e->setParent(parent);
			parent->addChild(e);
			e->setStartKey(actionLayer->startkey);
			e->setEndKey(actionLayer->endkey);
		}
		*/
	}
}
