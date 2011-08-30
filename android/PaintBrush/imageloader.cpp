#include "imageloader.h"
#include "type.h"
#include <string.h>
#ifdef ANDROID
#include "SkImageDecoder.h"
#include "SkBitmap.h"
#include "SkCanvas.h"
#include "SkString.h"
#include "SkStream.h"
#else
#include <IL/il.h>
#ifdef WIN32
#include <windows.h>
#endif
#endif
void save(Image image, const char* filename)
{
#if defined(WIN32)
    ILuint    imgId;
    ILenum    error;
    ilInit();
    ilGenImages(1, &imgId);
    ilBindImage(imgId);
	ilTexImage(image.width, image.height, 1, image.bpp, IL_RGB, IL_UNSIGNED_BYTE, image.data);
#if defined(WIN32)
    wchar_t fileWideChar[512];
    memset(fileWideChar, 0, sizeof(wchar_t) * 512);
    MultiByteToWideChar(CP_ACP, 0, filename, -1, fileWideChar, 511);    
    if(!ilSaveImage(fileWideChar))    
    {
        g_printerr("save error\n");
    }
#else
    ILboolean ret = ilSaveImage(filename);
#endif
	// We're done with the image, so let's delete it.
	ilDeleteImages(1, &imgId);
#endif
}
void save_ppm(ppm_t* p, const char* filename)
{
	Image image;
	image.x = 0;
	image.y = 0;
	image.width = p->width;
	image.height = p->height;
	image.data = p->col;
	image.bpp = 3;
	image.rowstride = p->width * 3;
	save(image, filename);
}
Image load(const char* filePath)
{
#if defined(WIN32)
    ILuint    imgId;
    ILenum    error;
    ilInit();
    ilGenImages(1, &imgId);
    ilBindImage(imgId);
#if defined(WIN32)
    wchar_t fileWideChar[512];
    memset(fileWideChar, 0, sizeof(wchar_t) * 512);
    MultiByteToWideChar(CP_ACP, 0, filePath, -1, fileWideChar, 511);    
    if(!ilLoadImage(fileWideChar))    
    {
        return Image();
    }
#else
    if(!ilLoadImage(filePath))
    {
        return NULL;
    }
#endif

    int width = ilGetInteger(IL_IMAGE_WIDTH);
    int height = ilGetInteger(IL_IMAGE_HEIGHT);
    int bpp = ilGetInteger(IL_IMAGE_BITS_PER_PIXEL);
    unsigned char* src = ilGetData();
    int pixelSize = bpp / 8;
    unsigned char* dst = new unsigned char[width * height * pixelSize];
    for(int y = height - 1 ; y >= 0 ; y--)
    {
        unsigned char* srcData = src + y * width * pixelSize;
        unsigned char* dstData = dst + (height - 1 - y) * width * pixelSize;
        memcpy(dstData, srcData, width * pixelSize);
    }   

    //memcpy(dst, src, width * height * pixelSize);
    //SE_ImageData* imageData = new SE_ImageData;
	Image imageData;
	imageData.width = width;
	imageData.height = height;
	imageData.bpp  = pixelSize;
	g_printerer("## width = %d, height = %d, bpp = %d ##\n", width, height, bpp);

	imageData.rowstride = width * pixelSize;
	imageData.data = dst;
    return imageData;
#else
    SkFILEStream fileStream(filePath);
    if(!fileStream.isValid()) {
        LOGI("error : can not open file %s\n",filePath);
        return NULL;
    }
    SkImageDecoder::Mode mode = SkImageDecoder::kDecodePixels_Mode;
    SkBitmap::Config prefConfig = SkBitmap::kNo_Config;
    SkImageDecoder* decoder = SkImageDecoder::Factory(&fileStream);
    SkBitmap* bitmap = new SkBitmap();
    if(!bitmap)
    {
         LOGI("Create bitmap fail,not enough memory!!!!!!!!!!\n");
    }
    else
    {
        LOGI("Create bitmap successs!!!!!!!!!!\n");
    }
    if(!decoder->decode(&fileStream, bitmap, prefConfig, mode))
    {
        printf("decode error \n");
    }
    LOGI("... image width X height = %d X %d\n", bitmap->width(), bitmap->height());
    LOGI("... image config = %d \n", bitmap->config());
    LOGI("... image rowbytes = %d\n", bitmap->rowBytes());
    int width = bitmap->width();
    int height = bitmap->height();
    int rowBytes = bitmap->rowBytes();
    int pixelFormat;
    if(bitmap->getConfig() == SkBitmap::kARGB_8888_Config)
    {
        pixelFormat = SE_ImageData::RGBA;
    }
    else if(bitmap->getConfig() == SkBitmap::kRGB_565_Config)
    {
        pixelFormat = SE_ImageData::RGB_565;
    }
    else
    {
        pixelFormat = SE_ImageData::RGBA;
    }
    char* data = (char*)bitmap->getPixels();
    char* newData = (char*)malloc(height * rowBytes);
    if(newData)
    {
        int i,j;
        int rowbytes = rowBytes;
        for(i = 0 ; i < height ; i++)
        {
            memcpy(newData + i * rowbytes, data + (height - 1 - i) * rowbytes, rowbytes);
        }
    }

    delete bitmap;

    SE_ImageData* imageData = new SE_ImageData;
    imageData->setWidth(width);
    imageData->setHeight(height);
    imageData->setBytesPerRow(rowBytes);
    imageData->setPixelFormat(pixelFormat);
    imageData->setData(newData);
    imageData->setCompressType(SE_ImageData::RAW);
    return imageData;
#endif
}