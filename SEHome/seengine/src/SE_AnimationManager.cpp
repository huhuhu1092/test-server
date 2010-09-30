#include "SE_AnimationManager.h"
#include "SE_Animation.h"
#include "SE_Application.h"
#include <list>
SE_AnimationID SE_AnimationManager::addAnimation(SE_Animation* anim)
{
    if(!anim)
        return SE_AnimationID::INVALID;
    SE_AnimationID animID = SE_ID::createAnimationID();
    mAnimationMap[animID] = anim;
    return animID;
}
void SE_AnimationManager::removeAnimation(const SE_AnimationID& animID)
{
    _AnimationMap::iterator it = mAnimationMap.find(animID);
    if(it != mAnimationMap.end())
    {
        SE_Animation* anim = it->second;
        anim->end();
        delete anim;
    }
}
SE_Animation*  SE_AnimationManager::getAnimation(const SE_AnimationID& animID)
{
    _AnimationMap::iterator it = mAnimationMap.find(animID);
    if(it != mAnimationMap.end())
    {
        SE_Animation* anim = it->second;
        return anim;
    }    
    return NULL;
}
void SE_AnimationManager::removeAllEndedAnimation()
{
    _AnimationMap::iterator it;
    std::list<SE_AnimationID> needToRemove;
    for(it = mAnimationMap.begin() ; it != mAnimationMap.end() ; it++)
    {
        SE_Animation* anim = it->second;
        if(anim->isEnd())
        {
            needToRemove.push_back(it->first);
        }
    }
    std::list<SE_AnimationID>::iterator itRemove;
    for(itRemove = needToRemove.begin() ; itRemove != needToRemove.end() ; itRemove++)
    {
        mAnimationMap.erase(*itRemove);
    }
}

void SE_AnimationManager::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    _AnimationMap::iterator it;
    for(it = mAnimationMap.begin() ; it != mAnimationMap.end() ; it++)
    {
        SE_Animation* anim = it->second;
        if(anim->isRunning())
        {
            anim->update(realDelta, simulateDelta);
        }
    }
}
