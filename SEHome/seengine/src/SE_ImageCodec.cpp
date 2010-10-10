#include "SE_ImageCodec.h"
#include "SE_Utils.h"
#include "SE_ImageData.h"
#include "SE_Log.h"
#include "utf/ConvertUTF.h"
#include <string.h>
#ifdef ANDROID
#include "SkImageDecoder.h"
#else
#include <IL/il.h>
#include <IL/ilu.h>
#include <IL/ilut.h>
#endif

SE_ImageData* SE_ImageCodec::load(const char* filePath)
{
    ILuint	imgId;
	ILenum	error;
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
	for(int y = height - 1 ; y >= 0 ; y--)
	{
		unsigned char* srcData = src + y * width * pixelSize;
		unsigned char* dstData = dst + (height - 1 - y) * width * pixelSize;
		memcpy(dstData, srcData, width * pixelSize);
	}
    //memcpy(dst, src, width * height * pixelSize);
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
    return imageData;
}
SE_ImageData* SE_ImageCodec::load(const wchar_t* filePath)
{
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
}
