#include "SSocket.h"
#include "SUtil.h"
#include <netinet/in.h> 
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include    <errno.h>
///////////////////////////
/*
class SNetAddress::SNetAddressImpl
{
public:
    struct in_addr mIp;
    uint16_t mPort;
};
*/
////////////////////////////
//SNetAddress SNetAddress::nullAddress(NULL, 0);
////////////////////////////
SSocket::SSocket(int fd)
{
    SASSERT(fd > 0);
    mSocket = fd;
}
SSocket::~SSocket()
{
    close(mSocket);
}
int SSocket::send(const unsigned char* data, int size)
{
    SASSERT(data != NULL && size > 0);
    int nleft;
    int nwritten;
    const unsigned char* ptr = data;
    nleft = size;
    while(nleft > 0)
    {
        nwritten = write(mSocket, ptr, nleft);
        if(nwritten <= 0)
        {
            if(nwritten < 0 && errno == EINTR)
                nwritten = 0;
            else
                return -1;
            nleft -= nwritten;
            ptr += nwritten;
        }
    }
    return size;
}
int SSocket::read(unsigned char* outBuffer, int size)
{
    int nleft;
    int nread;
    unsigned char* ptr = outBuffer;
    nleft = size;
    while(nleft > 0)
    {
        nread = ::read(mSocket, ptr, nleft);
        if(nread < 0)
        {
            if(errno == EINTR)
                nread = 0;
            else 
                return -1;
        }
        nleft -= nread;
        ptr += nread;
    }
    return size - nleft;
}
//////////////////////////////////////////////
SSocketServer::SSocketServer(int transferType, const SNetAddress& address) : mError(NO_ERROR)
{
    int s = 0;
    if(transferType == STREAM)
    {
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    if(s == -1)
    {
        mError = CREATE_ERROR;
        return;
    }
    mServer.setSocket(s);
    struct sockaddr_in servaddr;
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = address.getIp();
    servaddr.sin_port = address.getPort();
    int ret = bind(s, (const struct sockaddr *)&servaddr, sizeof(servaddr));
    if(ret == -1)
    {
        mError = BIND_ERROR;
        return;
    }
    mListenNum = 50;
    ret = listen(s, mListenNum); 
    if(ret == -1)
    {
        mError = LISTEN_ERROR;
    }
}
SSocketServer::~SSocketServer()
{
}

SClientProp SSocketServer::accept()
{
    struct sockaddr_in clientAddr;
    socklen_t clilen = sizeof(clientAddr);
    int clientSocket = ::accept(mServer.getSocket(), (sockaddr*)&clientAddr, &clilen);
    if(clientSocket == -1)
    {
        mError = ACCEPT_ERROR;
    }
    SSocket c(clientSocket);
    SClientProp ss( c, SNetAddress(clientAddr.sin_addr.s_addr, clientAddr.sin_port));
    return ss;
}

//////////////////////////////////////////////////////////////
SSocketClient::SSocketClient(int transferType, const SNetAddress& address)
{
    int s = 0;
    if(transferType == STREAM)
    {
        s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    }
    else if(transferType == DATAGRAM)
    {
        s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    }
    mRemote.setSocket(s);
    struct sockaddr_in remote;
    bzero(&remote, sizeof(remote));
    remote.sin_family = AF_INET;
    remote.sin_addr.s_addr = address.getIp();
    remote.sin_port = address.getPort();
    //bind(socket, &remote, sizeof(remote));
    connect(s, (const sockaddr*)&remote, sizeof(remote));
}
SSocketClient::~SSocketClient()
{}
int SSocketClient::send(const unsigned char* data, int size)
{
    return mRemote.send(data, size);
}
int SSocketClient::read(unsigned char* outBuffer, int size)
{
    return mRemote.read(outBuffer, size);
}

