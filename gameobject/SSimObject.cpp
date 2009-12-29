#include "SSimObject.h"
SSimObject::SSimObject(const char* name, STimeMS createTime, const SObjectID& objId ) : SObject(name)
{
    mCreateTime = createTime;
    mId = objId;
}
SSimObject::~SSimObject()
{}
SVector SSimObject::getPosition()
{
    return mPosition;
}
void SSimObject::setPosition(const SVector& vector)
{
    mPosition = vector;
}
void SSimObject::getDirection(SVector* outDir)
{
    SASSERT(outDir != NULL);
    outDir[0] = mDirection[0];
    outDir[1] = mDirection[1];
    outDir[2] = mDirection[2];
}
void SSimObject::getDirection(SVector& x, SVector& y, SVector& z)
{
    x = mDirection[0];
    y = mDirection[1];
    z = mDirection[2];
}
void SSimObject::setDirection(SVector* dir)
{
    SASSERT(dir != NULL);
    mDirection[0] = dir[0];
    mDirection[1] = dir[1];
    mDirection[2] = dir[2];
}
void SSimObject::setDirection(const SVector& x, const SVector& y, const SVector& z)
{
    mDirection[0] = x;
    mDirection[1] = y;
    mDirection[2] = z;
}
SVector SSimObject::getAccel()
{
    return mAccel;
}
void SSimObject::setAccel(const SVector& a)
{
    mAccel = ar;
}
SVector SSimObject::getVelocity()
{
    return mVelocity;
}
void SSimObject::setVelocity(const SVector& vel)
{
    mVelocity = vel;
}
STimeMS SSimObject::getCreateTime()
{
    return mCreatTime;
}
SObjectID SSimObject::getId()
{
    return mId;
}

