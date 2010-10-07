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
    //wchar_t* filePathUnicode = SE_Util::utf8ToUnicode(filePath);
    UTF16 filePathUnicode[512];
    memset(filePathUnicode, 0, sizeof(UTF16) * 512);
    int filePathLen = strlen(filePath);
    const UTF8* end = (const UTF8*)(filePath + filePathLen);
    UTF16* dstEnd = filePathUnicode + 512;
    ConversionFlags flag = strictConversion;
	const UTF8* srcStart = (const UTF8*)filePath;
	UTF16* dstStart = filePathUnicode;
    ConversionResult ret = ConvertUTF8toUTF16(&srcStart, end, &dstStart, dstEnd, flag);
    if(ret != conversionOK)
        return NULL;
    ilInit();
    iluInit();
    ilGenImages(1, &imgId);
    ilBindImage(imgId);
    if(!ilLoadImage((wchar_t*)filePathUnicode))
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
