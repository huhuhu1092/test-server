#ifndef SE_NETADDRESS_H
#define SE_NETADDRESS_H
#include "SE_Common.h"
class SE_NetAddress
{
public:
    //ip address and port which is network order int
    SE_NetAddress(const char* ip, uint16_t port);
    SE_NetAddress(uint32_t ip, uint16_t port);
    SE_NetAddress()
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
    friend bool operator ==(const SE_NetAddress&, const SE_NetAddress&);
    friend bool operator <(const SE_NetAddress&,const SE_NetAddress&);
    friend bool operator >(const SE_NetAddress&,const SE_NetAddress&);
    friend bool operator !=(const SE_NetAddress&,const SE_NetAddress&);
    bool toString(char* outIp, int size, uint16_t& outPort) const;
private:
    uint32_t mIp;
    uint16_t mPort;
};
#endif
