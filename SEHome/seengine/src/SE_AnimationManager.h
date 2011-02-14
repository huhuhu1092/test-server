#ifndef SE_ANIMATIONMANAGER_H
#define SE_ANIMATIONMANAGER_H
#include "SE_ID.h"
#include "SE_Time.h"
#include "SE_TreeStructManager.h"
#include <map>
#include "SE_Animation.h"
class SE_AnimationManager
{
public:
	enum {SIZE = 100};
	enum {MAX_SIZE = 1000};
	SE_AnimationManager() : mAnimationMap(SIZE, MAX_SIZE)
	{}
    SE_AnimationID addAnimation(SE_Animation* anim);
    SE_Animation* removeAnimation(const SE_AnimationID& animID);
    SE_Animation* getAnimation(const SE_AnimationID& animID);
    void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
	std::vector<SE_Animation*> removeAllEndedAnimation();
	void release(const SE_AnimationID& id, int delay = SE_RELEASE_DELAY);
	void release(SE_Animation* a, int delay = SE_RELEASE_DELAY);
    ~SE_AnimationManager();
private:
    SE_TreeStructManager<SE_Animation> mAnimationMap;
    //typedef std::map<SE_AnimationID, SE_Animation*> _AnimationMap;
    //_AnimationMap mAnimationMap;
};
#endif
