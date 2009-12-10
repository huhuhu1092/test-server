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
private:
    SNetAddress mAddress;
    SSocket mSocket;
    SMessageStream mInputStream;
    SMessageStream mOutputStream;
    bool mCanRemove;
    SMutex mCanRemoveMutex;
};
#endif
