#ifndef SSOCKET_H
#define SSOCKET_H
#include "SType.h"
enum {STREAM, DATAGRAM};
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
    //static SNetAddress nullAddress;
private:
    uint32_t mIp;
    uint16_t mPort;
    //class SNetAddressImpl;
    //auto_ptr<SNetAddressImpl> mImpl
};
class SSocket
{
public:
    SSocket(int fd);
    SSocket()
    {
        mSocket = -1;
    }
    ~SSocket();
    void setSocket(int fd)
    {
        mSocket = fd;
    }
    int getSocket() const
    {
        return mSocket;
    }
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
private:
    int mSocket;
};
struct SClientProp
{
    SClientProp(SSocket s, SNetAddress a) : socket(s), address(a)
    {
    }
    SClientProp()
    {}
    SSocket socket;
    SNetAddress address;
};
class SSocketServer
{
public:
    enum {NO_ERROR, CREATE_ERROR, BIND_ERROR, LISTEN_ERROR, ACCEPT_ERROR};
    SSocketServer(int transferType, const SNetAddress& address);
    ~SSocketServer();
    int getError()
    {
        return mError;
    }
    SClientProp accept();
private:
    SSocket mServer;
    int mListenNum;
    int mError;
};
class SSocketClient
{
public:
    SSocketClient(int tranferType,const SNetAddress& address);
    ~SSocketClient();
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
private:
    SSocket mRemote;
    int mError;
};
#endif
