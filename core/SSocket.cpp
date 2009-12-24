#include "SSocket.h"
#include "SUtil.h"
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include "SLog.h"
#if defined(WIN32)
#else
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#endif
///////////////////////////
////////////////////////////
//SNetAddress SNetAddress::nullAddress(NULL, 0);
////////////////////////////
SSocket::SSocket(SSOCKET_TYPE fd)
{
    SASSERT(fd > 0);
    mSocket = fd;
}
SSocket::~SSocket()
{
    //close(mSocket);
}
int SSocket::close()
{
#if defined(WIN32)
    ::closesocket(mSocket);
#else
    ::close(mSocket);
#endif
    return 1;
}
int SSocket::send(const unsigned char* data, int size)
{
    SASSERT(data != NULL && size > 0);
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
int SSocket::read(unsigned char* outBuffer, int size)
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
SSocketServer::SSocketServer(int transferType, const SNetAddress& address) : mError(S_NO_ERROR)
{
    SSOCKET_TYPE s = INVALID_SOCKET;
#if defined(WIN32)
    WSADATA wsd;
    if(WSAStartup(MAKEWORD(2, 2), &wsd ) != 0)
    {
        SLog::msg("#### socket startup error #####\n");
        mError = S_CREATE_ERROR;
        return;
    }
#else
#endif
    if(transferType == STREAM)
    {
        s = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    if(s == SOCKET_ERROR)
    {
        mError = S_CREATE_ERROR;
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
        mError = S_BIND_ERROR;
        return;
    }
    mListenNum = 50;
    ret = listen(s, mListenNum); 
    if(ret == SOCKET_ERROR)
    {
        mError = S_LISTEN_ERROR;
    }
}
SSocketServer::~SSocketServer()
{
    mServer.close();
}

SClientProp SSocketServer::accept()
{
    struct sockaddr_in clientAddr;
    socklen_t clilen = sizeof(clientAddr);
    int clientSocket = ::accept(mServer.getSocket(), (sockaddr*)&clientAddr, &clilen);
    if(clientSocket == INVALID_SOCKET)
    {
        mError = S_ACCEPT_ERROR;
        return SClientProp();
    }
#if defined(WIN32)
    u_long iMode = 1;
    ioctlsocket(clientSocket, FIONBIO, &iMode);
#else
    fcntl(clientSocket, F_SETFL, O_NONBLOCK );
#endif
    SSocket c(clientSocket);
    SClientProp ss( c, SNetAddress(clientAddr.sin_addr.s_addr, clientAddr.sin_port));
    return ss;
}

//////////////////////////////////////////////////////////////
SSocketClient::SSocketClient(int transferType, const SNetAddress& address)
{
    SSOCKET_TYPE s = INVALID_SOCKET;
    if(transferType == STREAM)
    {
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    if(s == SOCKET_ERROR)
    {
        mError = S_CREATE_ERROR;
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
        SLog::msg("### connect error ####\n");
        mError = S_CONNECT_ERROR;
    }

    //fcntl(s, F_SETFL, O_NONBLOCK );
}
SSocketClient::~SSocketClient()
{
    mRemote.close();
}
int SSocketClient::send(const unsigned char* data, int size)
{
    return mRemote.send(data, size);
}
int SSocketClient::read(unsigned char* outBuffer, int size)
{
    return mRemote.read(outBuffer, size);
}
int SSocketClient::getError()
{
    return mError;
}
