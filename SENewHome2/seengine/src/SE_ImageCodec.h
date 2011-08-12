#ifndef SE_IMAGECODEC_H
#define SE_IMAGECODEC_H

#ifdef ANDROID
#include "SkBitmap.h"
#endif
#include <wchar.h>
class SE_ImageData;
class SE_ImageCodec
{
public:
    //filePath use UTF-8 encoding
    static SE_ImageData* load(const char* filePath,int type = 1);//type is jpg by default
    //filePath use Unicode encoding
    static SE_ImageData* load(const wchar_t* filePath);

    static SE_ImageData* loadARGB(const char *filePath);
    

#ifdef ANDROID
    static SE_ImageData* load(SkBitmap* bitmap);
    static SE_ImageData* loadAsset(const char* filePath,int type = 1);//default type is jpg
#endif
    static void resizeImageData(SE_ImageData* imageData);

private:
    //load a hardware impressed image,need correct lib
    static SE_ImageData* loadPvrtc(const char *filedata,int filelengh,int type);
    static SE_ImageData* loadPvrtc(const char *filePath,int type);
};
#endif
