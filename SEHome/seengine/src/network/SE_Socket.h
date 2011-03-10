#ifndef SE_SOCKET_H
#define SE_SOCKET_H
#include "SE_Common.h"
#include "SE_NetAddress.h"
#if defined(WIN32)
#include <winsock2.h>
typedef SOCKET SE_SOCKET_TYPE;
typedef int socklen_t;
#else
typedef int SE_SOCKET_TYPE;
const int SOCKET_ERROR = -1;
const int INVALID_SOCKET = -1;
#endif
enum {SE_STREAM, SE_DATAGRAM};
enum {SE_NO_ERROR, SE_CREATE_ERROR, SE_BIND_ERROR, SE_LISTEN_ERROR, SE_CONNECT_ERROR, SE_ACCEPT_ERROR};
class SE_Socket
{
public:
    SE_Socket(SE_SOCKET_TYPE fd);
    SE_Socket()
    {
        mSocket = INVALID_SOCKET;
    }
    ~SE_Socket();
    void setSocket(SE_SOCKET_TYPE fd)
    {
        mSocket = fd;
    }
    SE_SOCKET_TYPE getSocket() const
    {
        return mSocket;
    }

    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int close();
private:
    SE_SOCKET_TYPE mSocket;
};
struct SE_ClientProp
{
    SE_ClientProp(SE_Socket s, SE_NetAddress a) : socket(s), address(a)
    {
    }
    SE_ClientProp()
    {}
    SE_Socket socket;
    SE_NetAddress address;
};
class SE_SocketServer
{
public:

    SE_SocketServer(int transferType, const SE_NetAddress& address);
    ~SE_SocketServer();
    int getError()
    {
        return mError;
    }
    SE_ClientProp accept();
private:
    SE_Socket mServer;
    int mListenNum;
    int mError;
};
class SE_SocketClient
{
public:
    //enum {NO_ERROR, CREATE_ERROR, CONNECT_ERROR};
    SE_SocketClient(int tranferType,const SE_NetAddress& address);
    ~SE_SocketClient();
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int getError();
    SE_SOCKET_TYPE getSocket()
    {
        return mRemote.getSocket();
    }
private:
    SE_Socket mRemote;
    int mError;
};
#endif
