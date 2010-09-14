#ifndef SE_IMAGECODEC_H
#define SE_IMAGECODEC_H
#include <wchar.h>
class SE_ImageData;
class SE_ImageCodec
{
public:
    //file path is use UTF-8 encoding
    static SE_ImageData* load(const char* filePath);
    static SE_ImageData* load(const wchar_t* filePath);
};
#endif
