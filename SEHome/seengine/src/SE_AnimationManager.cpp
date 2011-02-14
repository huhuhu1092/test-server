#include "SE_AnimationManager.h"
#include "SE_Animation.h"
#include "SE_Application.h"
#include <list>
SE_AnimationManager::~SE_AnimationManager()
{
	/*
    _AnimationMap::iterator it;
    std::list<SE_AnimationID> needToRemove;
    for(it = mAnimationMap.begin() ; it != mAnimationMap.end() ; it++)
    {
        SE_Animation* anim = it->second;
        needToRemove.push_back(it->first);
        delete anim;
    }
    std::list<SE_AnimationID>::iterator itRemove;
    for(itRemove = needToRemove.begin() ; itRemove != needToRemove.end() ; itRemove++)
    {
        mAnimationMap.erase(*itRemove);
    }
	*/
}
SE_AnimationID SE_AnimationManager::addAnimation(SE_Animation* anim)
{
    if(!anim)
        return SE_AnimationID::INVALID;
	return mAnimationMap.add(SE_AnimationID::NULLID, anim);
    //SE_AnimationID animID = SE_ID::createAnimationID();
    //mAnimationMap[animID] = anim;
    //return animID;
}
void SE_AnimationManager::release(const SE_AnimationID& id, int delay)
{
	mAnimationMap.release(id, delay);
}
void SE_AnimationManager::release(SE_Animation* a, int delay)
{
	mAnimationMap.release(a, delay);
}
SE_Animation* SE_AnimationManager::removeAnimation(const SE_AnimationID& animID)
{
	return mAnimationMap.remove(animID);
	/*
    _AnimationMap::iterator it = mAnimationMap.find(animID);
    if(it != mAnimationMap.end())
    {
        SE_Animation* anim = it->second;
        anim->end();
        delete anim;
		mAnimationMap.erase(it);
    }
	*/
}
SE_Animation*  SE_AnimationManager::getAnimation(const SE_AnimationID& animID)
{
	return mAnimationMap.find(animID);
	/*
    _AnimationMap::iterator it = mAnimationMap.find(animID);
    if(it != mAnimationMap.end())
    {
        SE_Animation* anim = it->second;
        return anim;
    }    
    return NULL;
	*/
}
static bool isRemoved(SE_Animation* a)
{
	if(a->isEnd())
		return true;
	else
		return false;
}
std::vector<SE_Animation*> SE_AnimationManager::removeAllEndedAnimation()
{
	std::vector<SE_Animation*> removedAnimation = mAnimationMap.remove_if(isRemoved);
    return removedAnimation;
	/*
    _AnimationMap::iterator it;
    std::list<SE_AnimationID> needToRemove;
    for(it = mAnimationMap.begin() ; it != mAnimationMap.end() ; it++)
    {
        SE_Animation* anim = it->second;
        if(anim->isEnd())
        {
            needToRemove.push_back(it->first);
            delete anim;
        }
    }
    std::list<SE_AnimationID>::iterator itRemove;
    for(itRemove = needToRemove.begin() ; itRemove != needToRemove.end() ; itRemove++)
    {
        mAnimationMap.erase(*itRemove);
    }
	*/
}
class _DoUpdate 
{
public:
	void operator()(SE_Animation* a) const
	{
        a->update(realDelta, simulateDelta);
	}
	SE_TimeMS realDelta;
	SE_TimeMS simulateDelta;
};
void SE_AnimationManager::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    _DoUpdate du;
	du.realDelta = realDelta;
	du.simulateDelta = simulateDelta;
	mAnimationMap.traverseList(du);
	/*
    _AnimationMap::iterator it;
    for(it = mAnimationMap.begin() ; it != mAnimationMap.end() ; it++)
    {
        SE_Animation* anim = it->second;
        if(anim->isRunning())
        {
            anim->update(realDelta, simulateDelta);
        }
    }
	*/
}
