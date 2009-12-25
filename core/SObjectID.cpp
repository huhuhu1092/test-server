#include "SObjectID.h"
#include "STime.h"
SObjectID SObjectID::create()
{
    STimeUS currentTime = STime::getCurrentTimeUS();
    return SObjectID(currentTime);
}
bool SObjectID::operator==(const SObjectID& right)
{
    return mId == right.mId;
}
bool SObjectID::operator<(const SObjectID& right)
{
    return mId < right.mId;
}
bool SObjectID::operator>(const SObjectID& right)
{
    return mId > right.mId;
}
bool SObjectID::operator!=(const SObjectID& right)
{
    return mId != right.mId;
}
SObjectID& SObjectID::SObjectID(const SObjectID& right)
{
    mId = right.mId;
    return *this;
}
void SObjectID::operator=(const SObjectID& right)
{
    mId = right.mId;
}
SObjectID::SObjectID(STimeUS m) : mId(m)
{}
