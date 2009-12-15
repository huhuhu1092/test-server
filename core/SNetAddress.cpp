#include "SNetAddress.h"
#include <netinet/in.h> 
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include    <errno.h>

SNetAddress::SNetAddress(const char* ip, uint16_t port)
{
    mPort = port;
    if(ip != NULL)
    {
        struct in_addr in_ip;
        int ret = inet_aton(ip, &in_ip);
        if(ret == 0)
        {
            mIp = htonl(INADDR_ANY);
        }
        else
        {
            mIp = in_ip.s_addr;
        }
    }
    else
    {
        mIp = htonl(INADDR_ANY);
    }
}

void SNetAddress::setIp(const char* ip)
{
    if(ip != NULL)
    {
        struct in_addr in_ip;
        int ret = inet_aton(ip, &in_ip);
        if(ret == 0)
        {
            mIp = htonl(INADDR_ANY);
        }
        else
        {
            mIp = in_ip.s_addr;
        }
    }
    else
    {
        mIp = htonl(INADDR_ANY);
    }

}

SNetAddress::SNetAddress(uint32_t ip, uint16_t port)
{
    mIp = ip;
    mPort = port;
}
bool operator ==(const SNetAddress& left, const SNetAddress& right)
{
    if(left.mIp == right.mIp && left.mPort == right.mPort)
        return true;
    else
        return false;
}
bool operator <(const SNetAddress& left, const SNetAddress& right)
{
    if(left.mIp < right.mIp)
        return true;
    else if(left.mIp == right.mIp && left.mPort < right.mPort)
        return true;
    else 
        return false;
}
bool operator >(const SNetAddress& left, const SNetAddress& right)
{
    if(left.mIp > right.mIp)
        return true;
    else if(left.mIp == right.mIp && left.mPort > right.mPort)
        return true;
    else 
        return false;
}
bool operator !=(const SNetAddress& left, const SNetAddress& right)
{
    return !(left == right);
}
bool SNetAddress::toString(char* outIp, int size, uint16_t& outPort)
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
}


