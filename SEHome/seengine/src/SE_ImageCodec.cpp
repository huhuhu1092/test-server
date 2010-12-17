#include "SE_ImageCodec.h"
#include "SE_Utils.h"
#include "SE_ImageData.h"
#include "SE_Log.h"
#include <string.h>
#ifdef ANDROID
#include "SkImageDecoder.h"
#include "SkBitmap.h"
#include "SkCanvas.h"
#include "SkString.h"
#include "SkStream.h"

#else
#include <IL/il.h>
#include <IL/ilu.h>
#include <windows.h>
//#include <IL/ilut.h>
#endif

SE_ImageData* SE_ImageCodec::load(const char* filePath, bool fliped)
{
#if defined(WIN32)
    ILuint	imgId;
    ilInit();
    iluInit();
    ilGenImages(1, &imgId);
    ilBindImage(imgId);
#if defined(WIN32)
    wchar_t fileWideChar[512];
	memset(fileWideChar, 0, sizeof(wchar_t) * 512);
	MultiByteToWideChar(CP_ACP, 0, filePath, -1, fileWideChar, 511);
    if(!ilLoadImage(fileWideChar))
    {
        return NULL;
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
	if(fliped)
	{
	    for(int y = height - 1 ; y >= 0 ; y--)
	    {
		    unsigned char* srcData = src + y * width * pixelSize;
		    unsigned char* dstData = dst + (height - 1 - y) * width * pixelSize;
		    memcpy(dstData, srcData, width * pixelSize);
		}
	}
	else
	{
		memcpy(dst, src, width * height * pixelSize);
	}
    SE_ImageData* imageData = new SE_ImageData;
    imageData->setWidth(width);
    imageData->setHeight(height);
    switch(pixelSize)
    {
    case 2:
        imageData->setPixelFormat(SE_ImageData::RGB_565);
        break;
    case 3:
        imageData->setPixelFormat(SE_ImageData::RGB);
        break;
    case 4:
        imageData->setPixelFormat(SE_ImageData::RGBA);
        break;
    default:
        LOGE("can not support the file format \n");
        break;
    }
    imageData->setBytesPerRow(width * pixelSize);
    imageData->setData((char*)dst);
    imageData->setCompressType(SE_ImageData::RAW);
	imageData->setIsFliped(fliped);
    return imageData;
#else
    SkFILEStream fileStream(filePath);
    SkImageDecoder::Mode mode = SkImageDecoder::kDecodePixels_Mode;
    SkBitmap::Config prefConfig = SkBitmap::kNo_Config;
    SkImageDecoder* decoder = SkImageDecoder::Factory(&fileStream);
    SkBitmap* bitmap = new SkBitmap();
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
SE_ImageData* SE_ImageCodec::load(const wchar_t* filePath, bool fliped)
{
#if defined(WIN32)
    ILuint	imgId;
	ILenum	error;
    ilInit();
	error = ilGetError();
    iluInit();
	error = ilGetError();
    ilGenImages(1, &imgId);
	error = ilGetError();
    ilBindImage(imgId);
	error = ilGetError();
    if(!ilLoadImage(filePath))
    {
		error = ilGetError();
        return NULL;
    }
    int width = ilGetInteger(IL_IMAGE_WIDTH);
    int height = ilGetInteger(IL_IMAGE_HEIGHT);
    int bpp = ilGetInteger(IL_IMAGE_BITS_PER_PIXEL);
    unsigned char* src = ilGetData();
    int pixelSize = bpp / 8;
    char* dst = new char[width * height * pixelSize];
    memcpy(dst, src, width * height * pixelSize);
    SE_ImageData* imageData = new SE_ImageData;
    imageData->setWidth(width);
    imageData->setHeight(height);
    switch(pixelSize)
    {
    case 2:
        imageData->setPixelFormat(SE_ImageData::RGB_565);
        break;
    case 3:
        imageData->setPixelFormat(SE_ImageData::RGB);
        break;
    case 4:
        imageData->setPixelFormat(SE_ImageData::RGBA);
        break;
    default:
        LOGE("can not support the file format \n");
        break;
    }
    imageData->setBytesPerRow(width * pixelSize);
    imageData->setData(dst);
    imageData->setCompressType(SE_ImageData::RAW);
    return imageData;
#else 
    return NULL;
#endif
}
