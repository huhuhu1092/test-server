#include "SE_ImageData.h"
SE_ImageDataPortion SE_ImageDataPortion::INVALID = SE_ImageDataPortion(0, 0, 0, 0);
////////////////////////////////
bool SE_ImageData::isCompressTypeByHardware()
{
    switch(mCompressType)
    {
    case ETC_RGB_4BPP:
        return true;
        break;
    case OGL_PVRTC2:
        return true;
        break;
    default:
        return false;
    }

}
