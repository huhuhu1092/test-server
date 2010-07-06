#include "SE_SpatialID.h"
#include <string>
struct SE_SpatialID::SE_SpatialIDImpl
{
    std::string id;
};
SE_SpatialID* SE_SpatialID::create(const char* name)
{
    SE_SpatialID* sid = new SE_SpatialID(name);
    return sid;
}
SE_SpatialID::SE_SpatialID()
{
    mImpl = new SE_SpatialID::SE_SpatialIDImpl;
}
SE_SpatialID::SE_SpatialID(const char* id)
{
    mImpl = new SE_SpatialID::SE_SpatialIDImpl;
    mImpl->id = id;
}
SE_SpatialID::~SE_SpatialID()
{
    delete mImpl;
}
bool operator==(const SE_SpatialID& left, const SE_SpatialID& right)
{
    return left.mImpl->id == right.mImpl->id;
}
bool operator<(const SE_SpatialID& left, const SE_SpatialID& right)
{
    return left.mImpl->id < right.mImpl->id;
}
bool operator>(const SE_SpatialID& left, const SE_SpatialID& right)
{
    return left.mImpl->id > right.mImpl->id;
}

