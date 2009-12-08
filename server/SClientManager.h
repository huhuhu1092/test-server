#ifndef SCLIENTMANAGER_H
#define SCLIENTMANAGER_H
class SNetAddress;
class SSocket;
class SClientManager
{
public:
    void addClient(const SNetAddress&, const SSocket&);
    void removeClient(const SNetAddress&);
private:

};
#endif
