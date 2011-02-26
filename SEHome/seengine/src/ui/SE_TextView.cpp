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

SE_TextView::SE_TextView()
{

}
SE_TextView::~SE_TextView()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    for(int i = 0 ; i < mCharImageIDArray.size() ; i++)
	{
		resourceManager->removeImageData(mCharImageIDArray[i]);
	}
}

void SE_TextView::spawn()
{
    FT_Library library;
    FT_Face face;
    FT_Glyph glyph;
    FT_UInt glyph_index;
    FT_Error error;
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_StringID fontResource = "D:\\model\\newhome3\\fonts\\DroidSansFallback.ttf";
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
   calculateRect(mPivotX, mPivotY, 0, 0);
}
void SE_TextView::setImageData(SE_Primitive* primitive)
{
    SE_ImageDataPortion dp;
	SE_ImageData* imageData = mCharImageArray[0];
	/*
	if(!imageData->isSizePower2())
	{
		imageData->getDataPower2();
	}
	dp.setX(imageData->getRealStartX());
	dp.setY(imageData->getRealStartY());
	dp.setWidth(imageData->getWidth());
	dp.setHeight(imageData->getHeight());
	*/
	primitive->setImageData(0, imageData, SE_TEXTURE0, NOT_OWN, dp);
}
void SE_TextView::setSurface(SE_Surface* surface)
{
    SE_ColorExtractShaderProperty* sp = new SE_ColorExtractShaderProperty;
    sp->setColorOperationMode(16);
	surface->setShaderProperty(sp);
    surface->setProgramDataID(COLOREXTRACT_SHADER);
	surface->setRendererID(COLOREXTRACT_RENDERER);
}
void SE_TextView::layout()
{}
SE_Spatial* SE_TextView::createSpatial()
{
	/*
	SE_Primitive* primitive = NULL;
	SE_PrimitiveID primitiveID;
	createPrimitive(primitive, primitiveID);
	setImageData(primitive);
	*/
	return createSpatialByImage();
}
