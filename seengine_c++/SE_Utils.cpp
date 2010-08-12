#include "SE_Utils.h"
#if defined(WIN32)
    #include <winsock2.h>
#else
    #include <unistd.h>
    #if defined(ANDOIRD)
        #include <sys/endian.h>
    #else
        #include <netinet/in.h>
    #endif
#endif
unsigned int SE_Util::host2NetInt32(unsigned int i)
{
	return htonl(i);
}
unsigned short SE_Util::host2NetInt16(unsigned short s)
{
	return htons(s);
}
unsigned int SE_Util::net2HostInt32(unsigned int i)
{
	return ntohl(i);
}
unsigned short SE_Util::net2HostInt16(unsigned short s)
{
	return ntohs(s);
}
void SE_Util::sleep(unsigned int s)
{
#if defined(WIN32)
    Sleep(s);
#else
    sleep(s)
#endif
}
