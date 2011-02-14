#include "SE_ColorEffectController.h"
#include "SE_TimeKey.h"
#include "SE_2DElement.h"
#include "SE_Image.h"
#include "SE_Utils.h"
#include "SE_URI.h"
/*
bool SE_ColorEffectFrame::isAddress(const SE_StringID& content)
{
	std::string str = content.getStr();
	if(str.empty())
		return false;
	int c = str[0];
	if(c == '&')
		return true;
	else 
		return false;
}
*/
SE_StringID SE_ColorEffectFrame::getURL(const SE_StringID& uri)
{
	SE_URI strURI(uri.getStr());
	return strURI.getURL();
}
/*
SE_StringID SE_ColorEffectFrame::getAddress(const SE_StringID& content)
{
	std::string str = content.getStr();
	std::string::size_type pos = str.find("&");
	SE_ASSERT(pos != std::string::npos);
	std::string address = str.substr(pos + 1);
	return SE_StringID(address.c_str());
}
*/
///////////////////////////
SE_Element* SE_ColorEffect::createElement()
{
    SE_ColorEffectElement* e = new SE_ColorEffectElement;
	e->setBackgroundAddress(mBackgroundID);
    e->setChannelAddress(mChannelID);
    e->setBackgroundAlphaAddress(mBackgroundAlpha);
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		SE_ColorEffectElement::_TextureMark m;
		_TextureColor* tc = mTextureColorData[i];
		if(tc)
		{
		    m.mColorAlphaAddress = tc->colorAlpha;
            m.mFnAddress = tc->fn.getStr();
            m.mTextureFnAddress = tc->texturefn;
		    m.mColorAddress = tc->mColor;
		    m.mColor2Address = tc->mColor2;
		    m.mTextureAddress = tc->mTextureID;
			m.valid = true;
		    e->setTextureMark(i, m);
		}
	}
	return e;
}
SE_ColorEffect::SE_ColorEffect()
{
    mTextureColorData.resize(MARK_NUM, NULL);
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
void SE_ColorEffectController::addKeyFrame(const SE_TimeKey& key, SE_ColorEffectFrame* frame)
{
    SE_KeyFrame<SE_ColorEffectFrame*>* keyframe = new SE_KeyFrame<SE_ColorEffectFrame*>;
    keyframe->data = frame;
    keyframe->key = key;
    mKeySequence.addKeyFrame(keyframe);
}
SE_ColorEffectFrame* SE_ColorEffectController::getKeyFrame(const SE_TimeKey& key) const
{
    SE_KeyFrame<SE_ColorEffectFrame*>* frame = mKeySequence.getKeyFrame(key);
    return frame->data;
}
std::vector<SE_TimeKey> SE_ColorEffectController::getKeys() const
{
	return mKeySequence.getKeys();
}