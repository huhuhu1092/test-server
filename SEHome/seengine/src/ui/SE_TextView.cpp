#include "SE_TextView.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Log.h"
#include "SE_Mesh.h"
#include "SE_ShaderProperty.h"
#include "SE_DataValueDefine.h"
#include <ft2build.h>
#include FT_FREETYPE_H 
#include FT_GLYPH_H

SE_CharView::SE_TextView()
{
    mCharImage = NULL;
    mFontSize = 0;
}
SE_CharView::~SE_TextView()
{
    /*
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    for(int i = 0 ; i < mCharImageIDArray.size() ; i++)
	{
		resourceManager->removeImageData(mCharImageIDArray[i]);
	}
    */
}

void SE_CharView::spawn()
{
    /*
    FT_Library library;
    FT_Face face;
    FT_Glyph glyph;
    FT_UInt glyph_index;
    FT_Error error;
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_StringID fontResource = "C:\\model\\newhome3\\fonts\\DroidSansFallback.ttf";
    error = FT_Init_FreeType(&library);
    if(error)
    {
        LOGI("can not init free type lib\n");
        return;
    }
    error = FT_New_Face(library, fontResource.getStr(), 0, &face);
    if(error == FT_Err_Unknown_File_Format)
    {
        LOGI("font format is not supported\n");
        return;
    }
    else if(error)
    {
        LOGI("new face error\n");
        return;
    }
    error = FT_Set_Pixel_Sizes(face, 0, 32);
    if(error)
    {
        LOGI("set font size error \n");
        return;
    }
#if defined(WIN32)
	TCHAR ch[] = L"È·¶¨"; 
    glyph_index = FT_Get_Char_Index(face, ch[0]);
#else
#endif
    error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);    
    if(error)
    {
        LOGI("load glyph error\n");
        return;
    }
   // error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
   error = FT_Get_Glyph(face->glyph, &glyph);
   if(error)
   {
       LOGI("get glyph error \n");
       return;
   }
   FT_Glyph_To_Bitmap(&glyph, ft_render_mode_normal, 0, 1);
   FT_BitmapGlyph bitmap_glyph = (FT_BitmapGlyph)glyph;
   FT_Bitmap& bitmap = bitmap_glyph->bitmap;
   int width = bitmap.width;
   int height = bitmap.rows;
   unsigned char* buf = new unsigned char[4 * width * height];
   memset(buf, 0, 4 * width * height);
   for(int i = 0 ; i < height; i++)
   {
	   for(int j = 0 ; j < width ; j++)
	   {
		   buf[i * width * 4 + 4 * j] = 255;
		   buf[i * width * 4 + 4 * j + 1] = 0;
           buf[i * width * 4 + 4 * j + 2] = 0;
		   buf[i * width * 4 + 4 * j + 3] = bitmap.buffer[i * width + j];
		   LOGI("%d  ", buf[i * width + 4 * j + 3]);
	   }
	   LOGI("\n");
   }
   LOGI("\n");
   SE_ImageData* imageData = new SE_ImageData;
   imageData->setWidth(width);
   imageData->setHeight(height);
   imageData->setBytesPerRow(4 * width);
   imageData->setData((char*)buf);
   imageData->setPixelFormat(SE_ImageData::RGBA);
   imageData->setCompressType(SE_ImageData::RAW);
   SE_ImageDataID imageDataID("txtque");
   resourceManager->setImageData(imageDataID, imageData);
   mCharImageIDArray.resize(1);
   mCharImageIDArray[0] = imageDataID;
   mCharImageArray.resize(1);
   mCharImageArray[0] = imageData;
   */
       /*
    int charNum = mText.getCharNum(); //unicode char num
    float left = 0, top = 0;
    SE_FontManager* fontManager = SE_Application::getInstanc()->getFontManager();
    SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_FontProperty fontProperty;
    fontProperty.fontSize = mFontSize;
    fontProperty.fontStyle = mCharStyle;
    fontProperty.fontColor = mFontColor;
    for(int i = 0 ; i < charNum ; i++)
    {
        SE_CharCode c = mText.getCharCode(i);
        SE_ImageData* image = fontManager->getImageData(c, fontProperty);
        if(image)
        {
            SE_TextView* t = new SE_TextView;
            t->setLeft(left);
            t->setTop(top);
            t->setWidth(image->getWidth());
            t->setHeight(image->getHeight());
            elementManager->add(getID(), t);
            t->mCharImageArray.resize(1);
            t->mCharImageArray[0] = image;
        }
    }
    */
    SE_FontManager* fontManager = SE_Application::getInstanc()->getFontManager();
    SE_FontProperty fontProperty;
    fontProperty.fontSize = mFontSize;
    fontProperty.fontStyle = mCharStyle;
    fontProperty.fontColor = mFontColor;
    if(mCharImage == NULL)
    {
        mCharImage = fontManager->getImageData(c, fontProperty);
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
    mFontSize = 0;
}
void SE_TextView::calculateTextBound(float& outWidth, float& outHeight)
{
    float textWidth = 0, textHeight = 0;
    for(int i = 0 ; i < charNum ; i++)
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
    std::vector<SE_ImageData*> charImageData;
    charImageData.resize(charNum);
    SE_FontManager* fontManager = SE_Application::getInstanc()->getFontManager();
    SE_FontProperty fontProperty;
    fontProperty.fontSize = mFontSize;
    fontProperty.fontStyle = mCharStyle;
    fontProperty.fontColor = mFontColor;
    for(int i = 0 ; i < charNum ; i++)
    {
        SE_CharCode c = mText.getCharCode(i);
        charImageData = fontManager->getImageData(c, fontProperty);
        SE_ASSERT(charImageData != NULL);
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
        elementManager->add(getID(), cv);
        cv->spawn();
    }
    calculate(mPivotX, mPivoty, 0, 0);
}
void SE_TextView::layout()
{}
SE_Spatial* SE_TextView::createSpatial()
{
	return SE_Widget::createSpatial();
}
