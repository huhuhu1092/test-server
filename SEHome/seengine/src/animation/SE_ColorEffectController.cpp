#include "SE_ColorEffectController.h"
#include "SE_Element.h"
#include "SE_Image.h"
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
SE_StringID SE_ColorEffectFrame::getAddress(const SE_StringID& content)
{
	std::string str = content.getStr();
	std::string::size_type pos = str.find("&");
	SE_ASSERT(pos != std::string::npos);
	std::string address = str.substr(pos + 1);
	return SE_StringID(address.c_str());
}
///////////////////////////
SE_Element* SE_ColorEffect::createElement()
{
    SE_ColorEffectElement* e = new SE_ColorEffectElement;
	if(isAddress(mBackgroundID))
		e->setBackgroundAddress(getAddress(mBackgroundID));
	else
		e->setBackgroundValue(mBackgroundID);
	if(isAddress(mChannelID))
		e->setChannelAddress(getAddress(mChannelID));
	else
		e->setChannelValue(mChannelID);
	if(isAddress(mBackgroundAlpha))
		e->setBackgroundAlphaAddress(getAddress(mBackgroundAlpha));
	else
		e->setBackgroundAlphaValue(atoi(mBackgroundAlpha.getStr()));
	for(int i = 0 ; i < MARK_NUM ; i++)
	{
		SE_ColorEffectElement::_TextureMark m;
		_TextureColor* tc = mTextureColorData[i];
		if(isAddress(tc->colorAlpha))
			m.mColorAlphaAddress = getAddress(tc->colorAlpha);
		else
			m.mColorAlphaValue = atoi(tc->colorAlpha.getStr());
		if(isAddress(tc->fn))
			m.mFnAddress = getAddress(tc->fn.getStr());
		else
			m.mFnValue = atoi(tc->fn.getStr());
		if(isAddress(tc->texturefn))
			m.mTextureFnAddress = getAddress(tc->texturefn);
		else
			m.mTextureFnValue = atoi(tc->texturefn.getStr());
		if(isAddress(tc->mColor))
			m.mColorAddress = getAddress(tc->mColor);
		else
		{
			SE_Util::SplitStringList strList = SE_Util::splitString(tc->mColor.getStr(), " ");
			SE_ASSERT(strList.size() == 3);
            std::string signstr = "+-";
			for(int i = 0 ; i < strList.size() ; i++)
            {
                std::string str = strList[i];
                std::string::size_type n = str.find_first_of(signstr, 0);
                std::string numstr;
				if(n == std::string::npos)
                {
					m.mColorValue[i].sign = SE_ColorEffectElement::SIGN_NO;
					m.mColorValue[i].value = atoi(str.c_str());
                }
                else if(str[n] == '+')
                {
                    m.mColorValue[i].sign = SE_ColorEffectElement::SIGN_PLUS;
                    numstr = str.substr(1);
					m.mColorValue[i].value = atoi(numstr.c_str());
                }
                else if(str[n] == '-')
                {
                    m.mColorValue[i].sign = SE_ColorEffectElement::SIGN_MINUS;
                    numstr = str.substr(1);
					m.mColorValue[i].value = atoi(numstr.c_str());
                }
            }
		}
		if(isAddress(tc->mTextureID))
			m.mTextureAddress = getAddress(tc->mTextureID);
		else
			m.mTextureValue = tc->mTextureID;
		e->setTextureMark(i, m);
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