#include "SE_TextView.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Log.h"
#include "SE_Mesh.h"
#include "SE_ShaderProperty.h"
#include "SE_DataValueDefine.h"
#include "SE_FontManager.h"
#include "SE_ElementManager.h"
#include <ft2build.h>
#include FT_FREETYPE_H 
#include FT_GLYPH_H

SE_CharView::SE_CharView()
{
    mCharImage = NULL;
    mFontSize = 0;
}
SE_CharView::~SE_CharView()
{

}

void SE_CharView::spawn()
{
    SE_FontManager* fontManager = SE_Application::getInstance()->getFontManager();
    SE_FontProperty fontProperty;
    fontProperty.fontSize = mFontSize;
    fontProperty.fontStyle = mCharStyle;
    fontProperty.fontColor = mFontColor;
    if(mCharImage == NULL)
    {
        mCharImage = fontManager->getImageData(mCharCode, fontProperty);
    }
    calculateRect(mPivotX, mPivotY, 0, 0);
}
void SE_CharView::setImageData(SE_Primitive* primitive)
{
    SE_ImageDataPortion dp;
	SE_ImageData* imageData = mCharImage;
	primitive->setImageData(0, imageData, SE_TEXTURE0, NOT_OWN, dp);
}
void SE_CharView::setSurface(SE_Surface* surface)
{
    SE_ColorExtractShaderProperty* sp = new SE_ColorExtractShaderProperty;
    sp->setColorOperationMode(16);
	surface->setShaderProperty(sp);
    surface->setProgramDataID(COLOREXTRACT_SHADER);
	surface->setRendererID(COLOREXTRACT_RENDERER);
}
void SE_CharView::layout()
{}
SE_Spatial* SE_CharView::createSpatial()
{
	SE_Spatial* spatial = createSpatialByImage();
    return spatial;
}
//////////////////
SE_TextView::SE_TextView()
{
}
void SE_TextView::calculateTextBound(float& outWidth, float& outHeight)
{
    float textWidth = 0, textHeight = 0;
    for(int i = 0 ; i < mCharImageData.size() ; i++)
    {
        textWidth += mCharImageData[i]->getWidth();
        if(textHeight < mCharImageData[i]->getHeight())
            textHeight = mCharImageData[i]->getHeight();
    }
    outWidth = textWidth;
    outHeight = textHeight;
}
void SE_TextView::spawn()
{
    int charNum = mText.getCharNum(); //unicode char num
    //std::vector<SE_ImageData*> charImageData;
    mCharImageData.resize(charNum);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_FontManager* fontManager = SE_Application::getInstance()->getFontManager();
    SE_FontProperty fontProperty;
	fontProperty.fontSize = mTextProperty[getState()].mFontSize;
    fontProperty.fontStyle = mTextProperty[getState()].mCharStyle;
    fontProperty.fontColor = mTextProperty[getState()].mFontColor;
    for(int i = 0 ; i < charNum ; i++)
    {
        SE_CharCode c = mText.getCharCode(i);
        mCharImageData[i] = fontManager->getImageData(c, fontProperty);
        SE_ASSERT(mCharImageData[i] != NULL);
    }
    float textWidth = 0, textHeight = 0;
    calculateTextBound(textWidth, textHeight);
    float startx = 0 , starty = 0;
    if(textWidth < mWidth)
        startx = (mWidth - textWidth) / 2;
    if(textHeight < mHeight)
        starty = (mHeight - textHeight) / 2;
    for(int i = 0 ; i < charNum ; i++)
    {
        SE_CharView* cv = new SE_CharView;
        cv->setImageData(mCharImageData[i]);
        cv->setPivotX(0);
        cv->setPivotY(0);
        cv->setMountPoint(startx, starty);
        cv->setWidth(mCharImageData[i]->getWidth());
        cv->setHeight(mCharImageData[i]->getHeight());
        elementManager->add(getID(), cv, true);
        cv->spawn();
		startx += mCharImageData[i]->getWidth();
    }
    calculateRect(mPivotX, mPivotY, 0, 0);
}
void SE_TextView::layout()
{}
SE_Spatial* SE_TextView::createSpatial()
{
	return SE_Widget::createSpatial();
}
