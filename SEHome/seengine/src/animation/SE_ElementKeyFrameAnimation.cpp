#include "SE_ElementKeyFrameAnimation.h"
#include "SE_Element.h"
#include "SE_Application.h"
SE_ElementKeyFrameAnimation::SE_ElementKeyFrameAnimation()
{
	mElement = NULL;
}
SE_ElementKeyFrameAnimation::~SE_ElementKeyFrameAnimation()
{}
void SE_ElementKeyFrameAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{
    if(frameIndex == getCurrentFrame())
        return;
    if(mElement)
        mElement->update(frameIndex);
}
SE_Animation* SE_ElementKeyFrameAnimation::clone()
{
	SE_ElementKeyFrameAnimation* anim = new SE_ElementKeyFrameAnimation;
	anim->mElement = mElement;
	SE_Animation::clone(anim);
	return anim;
}
void SE_ElementKeyFrameAnimation::onRun()
{
	setTimePerFrame(SE_Application::getInstance()->getFrameRate());
	setFrameNum(mElement->getKeyFrameNum());
}