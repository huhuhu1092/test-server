#include "SE_ColorEffectController.h"
void SE_ColorEffect::run()
{

}
SE_ColorEffect::SE_ColorEffect()
{
    mTextureColorData.resize(MARK_NUM, NULL);
    mAlpha = 255;
}
SE_ColorEffect::~SE_ColorEffect()
{
    std::vector<_TextureColor*>::iterator it;
    for(it = mTextureColorData.begin() ; it != mTextureColorData.end(); it++)
    {
        delete *it;
    }
}
////////////////////
void SE_ColorEffectController::addKeyFrame(unsigned int key, SE_ColorEffectFrame* frame)
{
    SE_KeyFrame<SE_ColorEffectFrame*>* keyframe = new SE_KeyFrame<SE_ColorEffectFrame*>;
    keyframe->data = frame;
    keyframe->key = key;
    mKeySequence.addKeyFrame(keyframe);
}
SE_ColorEffectFrame* SE_ColorEffectController::getKeyFrame(unsigned int key)
{
    SE_KeyFrame<SE_ColorEffectFrame*>* frame = mKeySequence.getKeyFrame(key);
    return frame->data;
}
std::vector<unsigned int> SE_ColorEffectController::getKeys()
{
	return mKeySequence.getKeys();
}