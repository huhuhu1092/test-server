#include "SE_Socket.h"
#if defined(WIN32)
//#ifndef _WIN32_WINNT            // Specifies that the minimum required platform is Windows Vista.
//#define _WIN32_WINNT 0x0600     // Change this to the appropriate value to target other versions of Windows.
//#endif
//#undef _INC_WINDOWS
//#ifndef WIN32_LEAN_AND_MEAN
//#define WIN32_LEAN_AND_MEAN
//#endif
#include <winsock2.h>
#include <windows.h>
typedef int socklen_t;
#else
typedef int SOCKET;
const int SOCKET_ERROR = -1;
const int INVALID_SOCKET = -1;
#endif
#include "SE_Utils.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include "SE_Log.h"
#if defined(WIN32)
#else
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#endif
///////////////////////////
////////////////////////////
//SNetAddress SNetAddress::nullAddress(NULL, 0);
////////////////////////////
SE_Socket::SE_Socket(SE_SOCKET_TYPE fd)
{
    SE_ASSERT(fd > 0);
    mSocket = fd;
	mIsNoneBlock = false;
}
SE_Socket::SE_Socket()
{
    mSocket = (SE_SOCKET_TYPE)INVALID_SOCKET;
	mIsNoneBlock = false;
}
SE_Socket::~SE_Socket()
{
}
int SE_Socket::close()
{
#if defined(WIN32)
    ::closesocket((SOCKET)mSocket);
#else
    ::close(mSocket);
#endif
    return 1;
}
int SE_Socket::send(const unsigned char* data, int size)
{
    SE_ASSERT(data != NULL && size > 0);
    int nleft;
    int nwritten;
    int totalWritten = 0;
    const unsigned char* ptr = data;
    nleft = size;
    while(nleft > 0)
    {
#if defined(WIN32)
        nwritten = ::send((SOCKET)mSocket, (const char*)ptr, nleft, 0);
        if(nwritten == SOCKET_ERROR && totalWritten == 0)
            return -1;
        else if(nwritten == SOCKET_ERROR && totalWritten > 0)
            return totalWritten;
#else
        nwritten = ::send(mSocket, (const char*)ptr, nleft, 0);
        if(nwritten <= 0)
        {
            if(nwritten < 0 && errno == EINTR)
                nwritten = 0;
            else
                return totalWritten;
        }
#endif
        nleft -= nwritten;
        ptr += nwritten;
        totalWritten += nwritten;
    }
    return totalWritten;
}
int SE_Socket::readRaw(unsigned char* outBuffer, int size)
{
	int nread = ::recv((SOCKET)mSocket, (char*)outBuffer, size, 0);
#if defined(WIN32)
	if(nread == SOCKET_ERROR)
	{
		return -1;
	}
	else
		return nread;
#else
#endif
}
int SE_Socket::read(unsigned char* outBuffer, int size)
{
    int nleft;
    int nread = 0;
    unsigned char* ptr = outBuffer;
    nleft = size;
    int totalRead = 0;
    while(nleft > 0)
    {
        //SLog::msg("#### read num = %d #########\n", nread);
#if defined(WIN32)
        nread = ::recv((SOCKET)mSocket, (char*)ptr, nleft, 0);
		LOGI("## read socket num : %d ###\n", nread);
        if(nread == SOCKET_ERROR)
        {
            if(totalRead == 0)
                return -1;
            else
                return totalRead;
        }
        else if(nread == 0)
        {
            if(totalRead == 0)
                return 0;
            else
                return totalRead;
        }
#else
        nread = ::recv(mSocket, (char*)ptr, nleft, 0);
        if(nread <= 0)
        {
            if(errno == EINTR)
                nread = 0;
            else 
            {
                if(totalRead == 0 && nread ==0)
                    return 0;
                else if(totalRead == 0 && nread < 0)
                    return nread;
                else
                    return totalRead;
            }
        }
#endif
        nleft -= nread;
        ptr += nread;
        totalRead += nread;
    }
	SE_ASSERT(totalRead == (size - nleft));
    return size - nleft;
}
void SE_Socket::setNoneBlock()
{
	mIsNoneBlock = true;
#if defined(WIN32)
    u_long iMode = 1;
    ioctlsocket((SOCKET)mSocket, FIONBIO, &iMode);
#else
    fcntl(mSocket, F_SETFL, O_NONBLOCK );
#endif
}
bool SE_Socket::isNoneBlock()
{
	return mIsNoneBlock;
}
//////////////////////////////////////////////
SE_SocketServer::SE_SocketServer(int transferType, const SE_NetAddress& address) : mError(SE_NO_ERROR)
{
	SE_SOCKET_TYPE s;
#if defined(WIN32)
    WSADATA wsd;
    if(WSAStartup(MAKEWORD(2, 2), &wsd ) != 0)
    {
        LOGI("#### socket startup error #####\n");
        mError = SE_CREATE_ERROR;
        return;
    }
#else
#endif
    if(transferType == SE_STREAM)
    {
        s = (SE_SOCKET_TYPE)::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == SE_DATAGRAM)
    {
        s = (SE_SOCKET_TYPE)socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
#if defined(WIN32)
    if(((SOCKET)s) == SOCKET_ERROR)
#else
	if(s == SOCKET_ERROR)
#endif
    {
        mError = SE_CREATE_ERROR;
        return;
    }
    mServer.setSocket(s);
    struct sockaddr_in servaddr;
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = address.getIp();
    servaddr.sin_port = address.getPort();
#if defined(WIN32)
    int ret = bind((SOCKET)s, (const struct sockaddr *)&servaddr, sizeof(servaddr));
#else
	int ret = bind(s, (const struct sockaddr *)&servaddr, sizeof(servaddr));
#endif
    if(ret == SOCKET_ERROR)
    {
        mError = SE_BIND_ERROR;
        return;
    }
    mListenNum = 50;
#if defined(WIN32)
    ret = listen((SOCKET)s, mListenNum); 
#else
	ret = listen(s, mListenNum); 
#endif
    if(ret == SOCKET_ERROR)
    {
        mError = SE_LISTEN_ERROR;
    }
}
SE_SocketServer::~SE_SocketServer()
{
    mServer.close();
}

SE_ClientProp SE_SocketServer::accept()
{
    struct sockaddr_in clientAddr;
    socklen_t clilen = sizeof(clientAddr);
#if defined(WIN32)
    SOCKET clientSocket = ::accept((SOCKET)mServer.getSocket(), (sockaddr*)&clientAddr, &clilen);
#else
    SOCKET clientSocket = ::accept(mServer.getSocket(), (sockaddr*)&clientAddr, &clilen);
#endif
    if(clientSocket == INVALID_SOCKET)
    {
        mError = SE_ACCEPT_ERROR;
        return SE_ClientProp();
    }
    SE_Socket c((SE_SOCKET_TYPE)clientSocket);
    SE_ClientProp ss(c, SE_NetAddress(clientAddr.sin_addr.s_addr, clientAddr.sin_port));
    return ss;
}

//////////////////////////////////////////////////////////////
SE_SocketClient::SE_SocketClient(int transferType, const SE_NetAddress& address)
{
    SE_SOCKET_TYPE s = (SE_SOCKET_TYPE)INVALID_SOCKET;
    char buf[100];
    uint16_t ppp;
    address.toString(buf, 100, ppp);
    LOGI("### ip address = %s , port = %d ###\n", buf, ppp);
    if(transferType == SE_STREAM)
    {
        LOGI("#### create socket ####\n");
        s = (SE_SOCKET_TYPE)socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == SE_DATAGRAM)
    {
        s = (SE_SOCKET_TYPE)socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
#if defined(WIN32)
    if(((SOCKET)s) == SOCKET_ERROR)
#else
	if(s == SOCKET_ERROR)
#endif
    {
        mError = SE_CREATE_ERROR;
        LOGI("#### create socket error \n");
        return;
    }
    mRemote.setSocket(s);
    struct sockaddr_in remote;
    memset(&remote, 0, sizeof(remote));
    remote.sin_family = AF_INET;
    remote.sin_addr.s_addr = address.getIp();
    remote.sin_port = address.getPort();
#if defined(WIN32)
    if(connect((SOCKET)s, (const sockaddr*)&remote, sizeof(remote)) == SOCKET_ERROR)
#else
	if(connect(s, (const sockaddr*)&remote, sizeof(remote)) == SOCKET_ERROR)
#endif
    {
        LOGI("### connect error ####\n");
        mError = SE_CONNECT_ERROR;
    }
    //fcntl(s, F_SETFL, O_NONBLOCK );
}
SE_SocketClient::~SE_SocketClient()
{
    mRemote.close();
}
int SE_SocketClient::send(const unsigned char* data, int size)
{
    return mRemote.send(data, size);
}
int SE_SocketClient::read(unsigned char* outBuffer, int size)
{
    return mRemote.read(outBuffer, size);
}
int SE_SocketClient::getError()
{
    return mError;
}
