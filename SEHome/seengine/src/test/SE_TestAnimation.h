#ifndef SE_TESTANIMATION_H
#define SE_TESTANIMATION_H
#include "SE_Animation.h"
#include "SE_ID.h"
#include <list>
class SE_Element;
class SE_TestAnimation : public SE_Animation
{
public:
	SE_TestAnimation();
    SE_TestAnimation(const char* imageTableName);
	virtual void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
    virtual void onRun();
	virtual SE_Animation* clone();
	void setElementID(const SE_ElementID& elementID)
	{
		mElementID = elementID;
	}
private:
	typedef std::list<SE_StringID> _ImageUnitList;
	std::list<_ImageUnitList> mImageSet;
	SE_ElementID mElementID;
	SE_Element* mElement;
private:
	void inc();
private:
    _ImageUnitList::iterator it2;
	std::list<_ImageUnitList>::iterator it1;
	bool bIt1End;
	bool bIt2End;
};
#endif