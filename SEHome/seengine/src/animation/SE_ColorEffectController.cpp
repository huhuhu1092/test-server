#include "SE_ColorEffectController.h"
#include "SE_Element.h"
#include "SE_Image.h"
SE_Vector3i SE_ChannelInput::COLOR_INVALID = SE_Vector3i(-1, -1, -1);
SE_Element* SE_ColorEffect::createElement(const SE_ColorEffectInput& input)
{
    SE_ColorEffectInput val;
	val.background = mBackgroundID;
	val.channel = mChannelID;
	val.alpha = mAlpha;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		_TextureColor* tc = mTextureColorData[i];
		if(tc)
		{
			val.channelInput[i].alpha = tc->alpha;
			val.channelInput[i].fn = tc->fn;
			val.channelInput[i].color = tc->mColor;
			val.channelInput[i].texture = tc->mTextureID;
			val.valid = true;
		}
	}
	if(input.background.isValid())
		val.background = input.background;
	if(input.channel.isValid())
		val.channel = input.channel;
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		const SE_ChannelInput* ci = &input.channelInput[i];
		if(ci->valid)
		{
			val.channelInput[i].alpha = ci->alpha != SE_ChannelInput::ALPHA_INVALID ? ci->alpha : val.channelInput[i].alpha;
			val.channelInput[i].fn = ci->fn != SE_ChannelInput::FN_INVALID ? ci->fn : val.channelInput[i].fn;
			val.channelInput[i].color = ci->color != SE_ChannelInput::COLOR_INVALID ? ci->color : val.channelInput[i].color;
			val.channelInput[i].texture = ci->texture.isValid() ? ci->texture : val.channelInput[i].texture;
		}
	}
	SE_ColorEffectImage* image = new SE_ColorEffectImage;
	image->setBackground(val.background);
	image->setChannel(val.channel);
	image->setAlpha(val.alpha);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		image->setAlpha(i, val.channelInput[i].alpha);
		image->setFunction(i, val.channelInput[i].fn);
		image->setColor(i, val.channelInput[i].color);
		image->setTexture(i, val.channelInput[i].texture);
	}
    SE_ColorEffectImageElement* e = new SE_ColorEffectImageElement(image);
	return e;
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
/////////////
SE_Element* SE_ColorEffectReload::createElement(const SE_ColorEffectInput& input)
{
	return NULL;
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