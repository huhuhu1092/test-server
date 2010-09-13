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
    //sleep(s);
#endif
}
int SE_Util::findHighBit(int v)
{
	unsigned int bit = 0;
	while(v > 1)
	{
		bit++;
		v >>= 1;
	}
	return bit;
}
bool SE_Util::isPower2(int v)
{
	if(v >= 0)
	    return (v & (v - 1)) == 0;
	else
		return 0;
}
int SE_Util::higherPower2(int v)
{
    if(isPower2(v))
        return v;
	return 1 << (findHighBit(v) + 1);
}
int SE_Util::lowerPower2(int v)
{
	if(isPower2(v))
		return v;
	return 1 << (findHighBit(v));
}