#ifndef SSIMOBJECT_H
#define SSIMOBJECT_H
#include "SObject.h"
#include "SVector.h"
#include "STime.h"
class SSimObject : public SObject
{
public:
    SSimObject(const char* name, STimeMS createTime, const SObjectID& objId );
    virtual ~SSimObject();
    SVector getPosition();
    void setPosition(const SVector& vector);
    void getDirection(SVector* outDir);
    void setDirection(SVector* dir);
    SVector getAccel();
    void setAccel(const SVector& a);
    SVector getVelocity();
    void setVelocity(const SVector& vel);
    STimeMS getCreateTime();
    SObjectID getId();
private:
    SVector mPosition;
    SVector[3] mDirection; // 0: x axis , 1 : y axis , 2: z axis
    SVector mAccel;
    SVector mVelocity;
    STimeMS mCreateTime;
    SObjectID mId;
    SBoundary mCollisionBound;
};
#endif
