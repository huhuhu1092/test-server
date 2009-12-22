#ifndef SUTIL_H
#define SUTIL_H
#include "SType.h"
#include <cassert>
#include <list>
using namespace std;
class SClient;
class SUtil
{
public:
    static uint16_t Net2HostInt16(uint16_t data);
    static uint16_t Host2NetInt16(uint16_t data);
    static uint32_t Net2HostInt32(uint32_t data);
    static uint32_t Host2NetInt32(uint32_t data);
};
typedef list<SClient*> SClientList;
#define SASSERT(x) assert(x)
#define S_DECLARE_NONECOPY(Class) \
    Class(const Class&); \
    Class& operator=(const Class&);
#endif
