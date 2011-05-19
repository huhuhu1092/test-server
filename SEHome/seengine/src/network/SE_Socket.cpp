#include "SE_Socket.h"
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
SE_Socket::SE_Socket(SOCKET fd)
{
    SE_ASSERT(fd > 0);
    mSocket = fd;
}
SE_Socket::~SE_Socket()
{
    //close(mSocket);
}
int SE_Socket::close()
{
#if defined(WIN32)
    ::closesocket(mSocket);
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
        nwritten = ::send(mSocket, (const char*)ptr, nleft, 0);
#if defined(WIN32)
        if(nwritten == SOCKET_ERROR && totalWritten == 0)
            return -1;
        else if(nwritten == SOCKET_ERROR && totalWritten > 0)
            return totalWritten;
#else
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
    return size;
}
int SE_Socket::read(unsigned char* outBuffer, int size)
{
    int nleft;
    int nread;
    unsigned char* ptr = outBuffer;
    nleft = size;
    int totalRead = 0;
    while(nleft > 0)
    {
        nread = ::recv(mSocket, (char*)ptr, nleft, 0);
        //SLog::msg("#### read num = %d #########\n", nread);
#if defined(WIN32)
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
    return size - nleft;
}
//////////////////////////////////////////////
SE_SocketServer::SE_SocketServer(int transferType, const SE_NetAddress& address) : mError(SE_NO_ERROR)
{
    SOCKET s = INVALID_SOCKET;
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
        s = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == SE_DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    if(s == SOCKET_ERROR)
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
    int ret = bind(s, (const struct sockaddr *)&servaddr, sizeof(servaddr));
    if(ret == SOCKET_ERROR)
    {
        mError = SE_BIND_ERROR;
        return;
    }
    mListenNum = 50;
    ret = listen(s, mListenNum); 
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
    SOCKET clientSocket = ::accept(mServer.getSocket(), (sockaddr*)&clientAddr, &clilen);
    if(clientSocket == INVALID_SOCKET)
    {
        mError = SE_ACCEPT_ERROR;
        return SE_ClientProp();
    }
#if defined(WIN32)
    u_long iMode = 1;
    ioctlsocket(clientSocket, FIONBIO, &iMode);
#else
    fcntl(clientSocket, F_SETFL, O_NONBLOCK );
#endif
    SE_Socket c(clientSocket);
    SE_ClientProp ss( c, SE_NetAddress(clientAddr.sin_addr.s_addr, clientAddr.sin_port));
    return ss;
}

//////////////////////////////////////////////////////////////
SE_SocketClient::SE_SocketClient(int transferType, const SE_NetAddress& address)
{
    SE_SOCKET_TYPE s = INVALID_SOCKET;
    char buf[100];
    uint16_t ppp;
    address.toString(buf, 100, ppp);
    LOGI("### ip address = %s , port = %d ###\n", buf, ppp);
    if(transferType == SE_STREAM)
    {
        LOGI("#### create socket ####\n");
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == SE_DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    if(s == SOCKET_ERROR)
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
    //bind(socket, &remote, sizeof(remote));
    if(connect(s, (const sockaddr*)&remote, sizeof(remote)) == SOCKET_ERROR)
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
