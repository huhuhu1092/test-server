#ifndef SNETADDRESS_H
#define SNETADDRESS_H
#include "SType.h"
class SNetAddress
{
public:
    //ip address and port which is network order int
    SNetAddress(const char* ip, uint16_t port);
    SNetAddress(uint32_t ip, uint16_t port);
    SNetAddress()
    {
        mIp = 0;
        mPort = 0;
    }
    void setIp(const char* ip);
    void setIp(uint32_t ip)
    {
        mIp = ip;
    }
    void setPort(int port)
    {
        mPort = port;
    }
    uint32_t getIp() const 
    {
        return mIp;
    }
    uint16_t getPort() const
    {
        return mPort;
    }
    friend bool operator ==(const SNetAddress&, const SNetAddress&);
    friend bool operator <(const SNetAddress&,const SNetAddress&);
    friend bool operator >(const SNetAddress&,const SNetAddress&);
    friend bool operator !=(const SNetAddress&,const SNetAddress&);
    bool toString(char* outIp, int size, uint16_t& outPort);
private:
    uint32_t mIp;
    uint16_t mPort;
};
#endif
