#include "SE_NetAddress.h"
#if defined(WIN32)
#include <winsock2.h>
#else
#include <netinet/in.h> 
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "SE_Log.h"
SE_NetAddress::SE_NetAddress(const char* ip, uint16_t port)
{
    mPort = htons(port);
	setIp(ip);
}

void SE_NetAddress::setIp(const char* ip)
{
    if(ip != NULL)
    {
#if defined(WIN32)
		unsigned long in_ip = inet_addr(ip);
		if(in_ip != INADDR_NONE)
		{
			mIp = in_ip;
		}
		else
		{
			mIp = htonl(INADDR_ANY);
		}
#else
        struct in_addr in_ip;
        int ret = inet_aton(ip, &in_ip);
        LOGI("## SE_NetAddress ret = %d $$\n", ret);
        if(ret == 0)
        {
            mIp = htonl(INADDR_ANY);
        }
        else
        {
            LOGI("#### ip = %d ####\n", in_ip.s_addr);
            mIp = in_ip.s_addr;
        }
#endif
    }
    else
    {
        mIp = htonl(INADDR_ANY);
    }
}

SE_NetAddress::SE_NetAddress(uint32_t ip, uint16_t port)
{
    mIp = ip;
    mPort = htons(port);
}
bool operator ==(const SE_NetAddress& left, const SE_NetAddress& right)
{
    if(left.mIp == right.mIp && left.mPort == right.mPort)
        return true;
    else
        return false;
}
bool operator <(const SE_NetAddress& left, const SE_NetAddress& right)
{
    if(left.mIp < right.mIp)
        return true;
    else if(left.mIp == right.mIp && left.mPort < right.mPort)
        return true;
    else 
        return false;
}
bool operator >(const SE_NetAddress& left, const SE_NetAddress& right)
{
    if(left.mIp > right.mIp)
        return true;
    else if(left.mIp == right.mIp && left.mPort > right.mPort)
        return true;
    else 
        return false;
}
bool operator !=(const SE_NetAddress& left, const SE_NetAddress& right)
{
    return !(left == right);
}
bool SE_NetAddress::toString(char* outIp, int size, uint16_t& outPort) const
{
    memset(outIp, 0, size);
    struct in_addr in_ip;
    in_ip.s_addr = mIp;
    char* strIP = inet_ntoa(in_ip);
    int strIpLen = strlen(strIP);
    int copyLen = strIpLen <= size ? strIpLen : size ;
    strncpy(outIp, strIP, copyLen);
    outIp[size - 1] = 0;
    outPort = ntohs(mPort);
	return true;
}


