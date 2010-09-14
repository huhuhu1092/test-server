#include "SE_ImageCodec.h"
#include "SE_Utils.h"
#include "SE_ImageData.h"
#include "SE_Log.h"
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
    wchar_t* filePathUnicode = SE_Util::utf8ToUnicode(filePath);
    ilInit();
    iluInit();
    ilGenImages(1, &imgId);
    ilBindImage(imgId);
    if(!ilLoadImage(filePathUnicode))
    {
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