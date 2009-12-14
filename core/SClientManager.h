#ifndef SCLIENTMANAGER_H
#define SCLIENTMANAGER_H
#include <memory>
class SNetAddress;
class SSocket;
class SClient;
class SCommandEventFactory;
//this class can not be inherited
using namespace std;
class SDoMapFunction
{
public:
    virtual void handle(const void* data) = 0;
};
class SDoMapCondition
{
public:
    virtual bool isSatisfied(const void* con) const= 0;
};
/////////////////////////
class SAddressEqualCon : public SDoMapCondition
{
public:
    bool isSatisfied(const void* con) const;
};
///////////////////////
class SAddressEqual : public SDoMapCondition
{
public:
    SAddressEqual(const SNetAddress& address) : mAddress(address)
    {}
    bool isSatisfied(const void* con) const;
private:
    const SNetAddress& mAddress;
};
/*
 *
 * DELETE_CLIENT command format:
 *  0: command id
 *  1 - 2: command length
 *  3 - 6: int ip address
 *  7 - 8: port   
 *
 * */
class SClientManager
{
public:
    enum COMMAND {DELETE_CLIENT, SEND_MSG};
    static SClientManager* getInstance();
    SClientManager();
    SClient* addClient(const SNetAddress&, const SSocket&);
    void removeClient(const SNetAddress&);
    // SClientManager will release factory. So dont do release by yourself.
    void init(SCommandEventFactory* factory);
    //handler must not handle long computing, otherwise it will lock client manager too long.
    //don't invoke this method in different thread with SClientManager master thread
    void handle(SDoMapCondition* condition, SDoMapFunction* handler) const; 
    void process();
private:
    SClientManager(const SClientManager&);
    SClientManager& operator=(const SClientManager&);
private:
    static SClientManager* mInstance;
    class Impl;
    Impl*  mImpl;
};
#endif
