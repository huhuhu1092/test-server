#include "SClient.h"
SClient::SClient(const SNetAddress& address, const SSocket& s) : mAddress(address), mSocket(s)
{}
void SClient::process()
{

}
bool SClient::canRemove()
{
    SAutoMutex mutex(&mCanRemoveMutex);
    return mCanRemove;
}
