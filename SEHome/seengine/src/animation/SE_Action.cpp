#include "SE_Action.h"
#include "SE_Element.h"
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
void SE_Action::createElement(SE_Element* parent)
{
	_ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* actionLayer = *it;
		if(actionLayer->startkey == 0)
		{
			SE_KeyFrame<SE_ActionUnit*>* kf = actionLayer->sequences.getKeyFrame(0);
			SE_ActionLayerElement* element = 
		}
		else
		{
            SE_ActionLayerElement* e = new SE_ActionLayerElement(actionLayer);
			e->setParent(parent);
			parent->addChild(e);
		}
	}
}
