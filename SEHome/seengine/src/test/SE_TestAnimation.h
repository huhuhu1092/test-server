#ifndef SE_TESTANIMATION_H
#define SE_TESTANIMATION_H
#include "SE_Animation.h"
#include "SE_ImageMap.h"
#include <list>
class SE_TestAnimation : public SE_Animation
{
public:
    SE_TestAnimation(const char* imageTableName);
	virtual void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
    virtual void onRun();
	virtual SE_Animation* clone();
	
private:
	typedef std::list<SE_ImageUnit> _ImageUnitList;
	std::list<_ImageUnitList> mImageSet;
};
#endif