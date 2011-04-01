#include "SE_ElementKeyFrameAnimation.h"
#include "SE_Element.h"
#include "SE_ElementManager.h"
#include "SE_Application.h"
SE_ElementKeyFrameAnimation::SE_ElementKeyFrameAnimation()
{
    elementManager = SE_GET_ELEMENTMANAGER();
}
SE_ElementKeyFrameAnimation::~SE_ElementKeyFrameAnimation()
{}
void SE_ElementKeyFrameAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{
    if(frameIndex == getCurrentFrame())
        return;

    SE_Element* element = elementManager->get(mElementID);
    if(element)
    {
        element->update(frameIndex);
        element->setRenderTargetID(element->getRenderTargetID());
        element->setRenderTargetSeq(element->getRenderTargetSeq());
        element->setSceneRenderSeq(element->getSceneRenderSeq());
        //SE_Element* root = element->getRoot();
        //SE_ASSERT(root != NULL);
        //root->layout();
        element->updateSpatial(true);
    }
}
SE_Animation* SE_ElementKeyFrameAnimation::clone()
{
	SE_ElementKeyFrameAnimation* anim = new SE_ElementKeyFrameAnimation;
	anim->mElementID = mElementID;
	SE_Animation::clone(anim);
	return anim;
}
void SE_ElementKeyFrameAnimation::onRun()
{
	setTimePerFrame(SE_Application::getInstance()->getFrameRate());
    SE_Element* element = elementManager->get(mElementID);
    if(element)
    {
	    setFrameNum(element->getKeyFrameNum());
    }
}
