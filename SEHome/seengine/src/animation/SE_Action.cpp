#include "SE_Action.h"
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
    SE_StringID id    
};
SE_Action::SE_Action()
{
	mRenderMode = REENDER_TO_BUFFER;
}
SE_Action::~SE_Action()
{}
static bool SE_Action::compareActionLayer(_ActionLayer* first, _ActionLayer* second)
{
	if(first->layer < second->layer)
		return true;
	else
		return false;
}
void SE_Action::sort()
{
	mActionLayerList.sort(&SE_Action::compareActionLayer);
	typename _ActionLayerList::iterator it;
	for(it = mActionLayerList.begin() ; it != mActionLayerList.end() ; it++)
	{
		_ActionLayer* al = *it;
		unsigned int sk, ek;
		std::vector<unsigned int> keys = al->sequences.getKeys();
		al->startkey = min_element(keys.begin(), keys.end());
		al->endkey = max_element(keys.begin(), keys.end());
	}
}
void SE_Action::addKeyFrame(SE_KeyFrame<SE_ActionUnit*>* keyframe)
{
	typename _ActionLayerList::iterator it;
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
void SE_Action::removeActionUnit(const SE_StringID& auID)
{}
SE_ActionUnit* SE_Action::getActionUnit(const SE_StringID& auID)
{}
    
