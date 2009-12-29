#ifndef SAREA_H
#define SAREA_H
#include <vector>
#include <list>
using namespace vector;
class SSimObject;
class SObjectID;
class SArea : public SObject
{
public:
    SArea();
    ~SArea();
    bool contain(SSimObject *);
    bool contain(const SObjectID& objId);
private:
    vector<SArea*> mChildren;
    list<SSimObject*> mContainedSimObject;
};
#endif
