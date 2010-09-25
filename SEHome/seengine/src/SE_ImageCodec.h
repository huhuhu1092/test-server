#ifndef SE_IMAGECODEC_H
#define SE_IMAGECODEC_H
#include <wchar.h>
class SE_ImageData;
class SE_ImageCodec
{
public:
    //filePath use UTF-8 encoding
	static SE_ImageData* load(const char* filePath);
	//filePath use Unicode encoding
    static SE_ImageData* load(const wchar_t* filePath);
};
#endif
