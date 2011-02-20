#include "SE_ImageData.h"
#include "SE_Utils.h"
SE_ImageDataPortion SE_ImageDataPortion::INVALID = SE_ImageDataPortion(0, 0, 0, 0);
////////////////////////////////
bool SE_ImageData::isCompressTypeByHardware()
{
    return mCompressType == ETC1;
}
bool SE_ImageData::isSizePower2() const
{
    return SE_Util::isPower2(mWidth) && SE_Util::isPower2(mHeight);
}
char* SE_ImageData::getDataPower2()
{
    if(isSizePower2())
        return mData;
    if(mDataPower2)
        return mDataPower2;
    int pixelSize = getPixelSize();
    int power2Width = SE_Util::higherPower2(mWidth);
    int power2Height = SE_Util::higherPower2(mHeight);
    int size = power2Width * power2Height * getPixelSize();   
    if(size == 0)
        return NULL;
    char* data = new char[size];
    if(!data)
        return NULL;
    memset(data, 0, size);
    char* src = getData();
    int starty = (power2Height - mHeight) >> 1;
    int startx = (power2Width - mWidth) >> 1;
    for(int y = 0 ; y < mHeight ; y++)
    {
        char* ydst = &data[(starty + y) * power2Width * pixelSize];
        memcpy(&ydst[startx * pixelSize], src, mWidth * pixelSize);
        src += mWidth * pixelSize;
    }    
    mWidthPower2 = power2Width;
    mHeightPower2 = power2Height;
    mBytesPerRowPower2 = power2Width * pixelSize;
    mDataPower2 = data;
    mRealStartX = startx;
    mRealStartY = starty;
    return mDataPower2;
}
