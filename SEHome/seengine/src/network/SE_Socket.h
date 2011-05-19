#ifndef SE_SOCKET_H
#define SE_SOCKET_H
#include "SE_Common.h"
#include "SE_NetAddress.h"
#if defined(WIN32)
#ifndef _WIN32_WINNT            // Specifies that the minimum required platform is Windows Vista.
#define _WIN32_WINNT 0x0600     // Change this to the appropriate value to target other versions of Windows.
#endif

#undef _INC_WINDOWS
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <winsock2.h>


//#include <windows.h>

//typedef SOCKET SE_SOCKET_TYPE;
typedef int socklen_t;
#else
typedef int SOCKET;
const int SOCKET_ERROR = -1;
const int INVALID_SOCKET = -1;
#endif
enum {SE_STREAM, SE_DATAGRAM};
enum {SE_NO_ERROR, SE_CREATE_ERROR, SE_BIND_ERROR, SE_LISTEN_ERROR, SE_CONNECT_ERROR, SE_ACCEPT_ERROR};
class SE_Socket
{
public:
	SE_Socket(SOCKET fd);
    SE_Socket()
    {
        mSocket = INVALID_SOCKET;
    }
    ~SE_Socket();
    void setSocket(SOCKET fd)
    {
        mSocket = fd;
    }
    SOCKET getSocket() const
    {
        return mSocket;
    }

    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int close();
private:
    SOCKET mSocket;
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
    SOCKET getSocket()
    {
        return mRemote.getSocket();
    }
private:
    SE_Socket mRemote;
    int mError;
};
#endif
