#ifndef SOBJECTID_H
#define SOBJECTID_H
#include "SType.h"
class SObjectID
{
public:
    static SObjectID create();
    bool operator==(const SObjectID&);
    bool operator<(const SObjectID&);
    bool operator>(const SObjectID&);
    bool operator!=(const SObjectID&);
    SObjectID& SObjectID(const SObjectID&);
    void operator=(const SObjectID&);
private:
    SObjectID(STimeUS m);
private:
    STimeUS mId;
};
#endif
