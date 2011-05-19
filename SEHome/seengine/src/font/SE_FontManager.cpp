#include "SE_FontManager.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageData.h"
#include "SE_Log.h"
#include <ft2build.h>
#include FT_FREETYPE_H 
#include FT_GLYPH_H

SE_FontManager::SE_FontManager()
{}
SE_FontManager::~SE_FontManager()
{}
SE_CharFontMap* SE_FontManager::loadCharStyle(const SE_CharStyle& cs)
{
	return NULL;
}
void SE_FontManager::setStyle(const SE_CharStyle& cs, char* fontData, const std::string& fontType)
{
    SE_CharFontMap* charFontMap = mFontStyleMap.getItem(cs);
    if(charFontMap == NULL)
    {
        charFontMap = new SE_CharFontMap;
        mFontStyleMap.setItem(cs, charFontMap);
    }
    char* data = charFontMap->getProperty().fontData;
    if(data)
    {
        delete[] data;
    }
    charFontMap->getProperty().fontData = fontData;
    charFontMap->getProperty().fontType = fontType;
}

void SE_FontManager::setStyle(const SE_CharStyle& cs, const std::string& fontFileName, const std::string& fontType)
{
    char* data = NULL;
    int len = 0; 
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->loadFontData(fontFileName.c_str(), data, len);
    if(!data)
        return;
    SE_CharFontMap* charFontMap = mFontStyleMap.getItem(cs);
    if(!charFontMap)
    {
        charFontMap = new SE_CharFontMap;
        mFontStyleMap.setItem(cs, charFontMap);
    }
    SE_FontResData& fontResData = charFontMap->getProperty();
    if(fontResData.fontData)
    {
        delete[] fontResData.fontData;
    }
    fontResData.fontData = data;
    fontResData.fontDataLen = len;
    fontResData.fontType = fontType;
}
SE_ImageData* SE_FontManager::initChar(SE_CharFontMap* charFontMap, const SE_CharCode& ch, int fontSize, const SE_Vector3i& fontColor)
{
    SE_FontResData& fontResData = charFontMap->getProperty();
    SE_ASSERT(fontResData.fontData);
    if(fontResData.fontType == "TTF")
    {
        FT_Library library;
        FT_Face face;
        FT_Glyph glyph;
        FT_UInt glyph_index;
        FT_Error error;
        error = FT_Init_FreeType(&library);
        if(error)
        {
            LOGI("can not init free type lib\n");
            return NULL;
        }
        error = FT_New_Memory_Face(library, (const FT_Byte *)fontResData.fontData, fontResData.fontDataLen, 0, &face);
        if(error == FT_Err_Unknown_File_Format)
        {
            LOGI("font format is not supported\n");
            FT_Done_FreeType( library );
            return NULL;
        }
        else if(error)
        {
            LOGI("new face error\n");
            FT_Done_FreeType( library );
            return NULL;
        }
        error = FT_Set_Pixel_Sizes(face, 0, fontSize);
        if(error)
        {
            LOGI("set font size error \n");
            FT_Done_Face( face );
            FT_Done_FreeType( library );
            return NULL;
        }
        glyph_index = FT_Get_Char_Index(face, ch.toUInt());
        error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);    
        if(error)
        {
            LOGI("load glyph error\n");
            FT_Done_Face( face );
            FT_Done_FreeType( library );
            return NULL;
        }
        error = FT_Get_Glyph(face->glyph, &glyph);
        if(error)
        { 
            LOGI("get glyph error \n");
            FT_Done_Face( face );
            FT_Done_FreeType( library );
            return NULL;
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
		        buf[i * width * 4 + 4 * j] = fontColor.x;
		        buf[i * width * 4 + 4 * j + 1] = fontColor.y;
                buf[i * width * 4 + 4 * j + 2] = fontColor.z;
		        buf[i * width * 4 + 4 * j + 3] = bitmap.buffer[i * width + j];
		        //LOGI("%d  ", buf[i * width * 4 + 4 * j + 3]);
	        }
	        //LOGI("\n");
        }
        //LOGI("\n");
        SE_ImageData* imageData = new SE_ImageData;
        imageData->setWidth(width);
        imageData->setHeight(height);
        imageData->setBytesPerRow(4 * width);
        imageData->setData((char*)buf);
        imageData->setPixelFormat(SE_ImageData::RGBA);
        imageData->setCompressType(SE_ImageData::RAW);
        FT_Done_Face( face );
        FT_Done_FreeType( library );
        return imageData;
    }
    else
    {
        //TODO: other type font need surport;
        LOGI("font type : %s : not support\n", fontResData.fontType.c_str());
        return NULL;
    }
}
SE_ImageData* SE_FontManager::getImageData(const SE_CharCode& c, const SE_FontProperty& p)
{
    SE_CharFontMap* charFontMap = mFontStyleMap.getItem(p.fontStyle);
    if(charFontMap == NULL)
    {
        return NULL;
    }
    SE_SizeFontMap* sizeFontMap = charFontMap->getItem(c);
    SE_ImageData* imageData = NULL;
    if(!sizeFontMap)
    {
        sizeFontMap = new SE_SizeFontMap;
        charFontMap->setItem(c, sizeFontMap);
        imageData = initChar(charFontMap, c, p.fontSize, p.fontColor);
        if(imageData)
        {
            SE_ColorFontMap* colorFontMap = new SE_ColorFontMap;
            colorFontMap->setItem(p.fontColor, imageData);
            sizeFontMap->setItem(p.fontSize, colorFontMap);
            return imageData;
        }
        else
        {
            return NULL;
        }
    }
    SE_ColorFontMap* colorFontMap = sizeFontMap->getItem(p.fontSize);
    if(!colorFontMap)
    {
        colorFontMap = new SE_ColorFontMap;
        imageData = initChar(charFontMap, c, p.fontSize, p.fontColor);
        if(imageData)
        {
            sizeFontMap->setItem(p.fontSize, colorFontMap);
            colorFontMap->setItem(p.fontColor, imageData);
            return imageData;
        }
        else
        {
            delete colorFontMap;
            return NULL;
        }
    } 
	else
	{
		imageData = colorFontMap->getItem(p.fontColor);
		if(!imageData)
		{
            imageData = initChar(charFontMap, c, p.fontSize, p.fontColor);
            if(imageData)
            {
                colorFontMap->setItem(p.fontColor, imageData);
            }
            else
            {
                delete colorFontMap;
            }
		}
		return imageData;
	}
}
