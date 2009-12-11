#ifndef SCLIENT_H
#define SCLIENT_H
#include "SNetAddress.h"
#include "SSocket.h"
#include "SMessageStream.h"
#include "SMutex.h"
class SClient
{
public:
    SClient(const SNetAddress& address, const SSocket& s);
    bool canRemove();
    void process();
    SSocket* getSocket()
    {
        return &mSocket;
    }
    SNetAddress getNetAddress()
    {
        return mAddress;
    }
    SMessageStream* getInputStream()
    {
        return &mInputStream;
    }
    SMessageStream& getOutputStream()
    {
        return mOutputStream;
    }
    void readData();
    void writeData();
private:
    SNetAddress mAddress;
    SSocket mSocket;
    SMessageStream mInputStream;
    SMessageStream mOutputStream;
    bool mCanRemove;
    SMutex mCanRemoveMutex;
};
#endif
