#ifndef SE_ELEMENTKEYFRAMEANIMATION_H
#define SE_ELEMENTKEYFRAMEANIMATION_H
#include "SE_Animation.h"
class SE_Element;
class SE_ElementManager;
class SE_ElementKeyFrameAnimation : public SE_Animation
{
public:
    SE_ElementKeyFrameAnimation();
    ~SE_ElementKeyFrameAnimation();
    void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
	void onRun();
    SE_Animation* clone();
    void setElement(const SE_ElementID& elementID)
    {
        mElementID = elementID;
    }
    SE_ElementID getElement() const
    {
        return mElementID;
    }
private:
    SE_ElementID mElementID;
    SE_ElementManager* elementManager;
};
#endif
