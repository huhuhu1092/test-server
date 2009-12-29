#ifndef SSIMOBJECTGROUP_H
#define SSIMOBJECTGROUP_H
#include "SObject.h"
#include <list>
using namespace std;
class SSimObjectGroup : public SSimObject
{
public:
    SSimObjectGroup(const char* name, STimeMS createTime, const SObjectID& objId );
    virtual ~SSimObjectGroup();
    void addObject(SSimObject* obj);
    void removeObject(const SObjectID& objId);
    void removeObject(SSimObject* obj);
    SSimObject* getObject(const SObjectID& objId);
private:
    list<SSimObject*> mObjectList;
};
#endif
