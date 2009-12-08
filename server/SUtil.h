#ifndef SUTIL_H
#define SUTIL_H
#include "SType.h"
#include <cassert>
class SUtil
{
public:
    static uint16_t Net2HostInt16(uint16_t data);
    static uint16_t Host2NetInt16(uint16_t data);
    static uint32_t Net2HostInt32(uint32_t data);
    static uint32_t Host2NetInt32(uint32_t data);
};
#define SASSERT(x) assert(x)
#endif
