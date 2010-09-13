#include "SE_ImageData.h"
SE_ImageDataPortion SE_ImageDataPortion::INVALID = SE_ImageDataPortion(0, 0, 0, 0);
////////////////////////////////
bool SE_ImageData::isCompressTypeByHardware()
{
    return compressType == ETC1;
}
