#ifndef SE_IMAGEDATA_H
#define SE_IMAGEDATA_H
class SE_ImageData
{
public:
    enum {INVALID, RGB_565, RGB, RGBA};
    SE_ImageData()
    {
        width = 0;
        height = 0;
        pixelFormat = INVALID;
        bytesPerRow = 0;
        data = NULL;
    }
    ~SE_ImageData()
    {
        if(data)
            delete[] data;
    }
public:
    int height;
    int width;
    int pixelFormat;
    int bytesPerRow;
    char* data;
};
#endif
