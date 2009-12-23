#include "SUtil.h"
#if defined(WIN32)
#include <winsock2.h>
#else
#include <netinet/in.h>
#endif
uint16_t SUtil::Net2HostInt16(uint16_t data)
{
    return ntohs(data);
}
uint16_t SUtil::Host2NetInt16(uint16_t data)
{
    return htons(data);
}
uint32_t SUtil::Net2HostInt32(uint32_t data)
{
    return ntohl(data);
}
uint32_t SUtil::Host2NetInt32(uint32_t data)
{
    return htonl(data);
}

