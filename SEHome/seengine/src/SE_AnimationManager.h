#ifndef SE_ANIMATIONMANAGER_H
#define SE_ANIMATIONMANAGER_H
#include "SE_ID.h"
#include "SE_Time.h"
#include <map>
class SE_Animation;
class SE_AnimationManager
{
public:
    SE_AnimationID addAnimation(SE_Animation* anim);
    void removeAnimation(const SE_AnimationID& animID);
    SE_Animation* getAnimation(const SE_AnimationID& animID);
    void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    void removeAllEndedAnimation();
    ~SE_AnimationManager();
private:
    typedef std::map<SE_AnimationID, SE_Animation*> _AnimationMap;
    _AnimationMap mAnimationMap;
};
#endif
