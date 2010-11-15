#ifndef SE_ELEMENTKEYFRAMEANIMATION_H
#define SE_ELEMENTKEYFRAMEANIMATION_H
#include "SE_Animation.h"
class SE_Element;
class SE_ElementKeyFrameAnimation : public SE_Animation
{
public:
    SE_ElementKeyFrameAnimation();
    ~SE_ElementKeyFrameAnimation();
    void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
	void onRun();
    SE_Animation* clone();
    SE_Element* getElement()
    {
        return mElement;
    }
    void setElement(SE_Element* e)
    {
        mElement = e;
    }
private:
    SE_Element* mElement;
};
#endif
