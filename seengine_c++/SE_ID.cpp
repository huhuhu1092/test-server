#include "SE_ID.h"
struct SE_CommonID::_Impl
{
    std::string id;
};
SE_CommonID::SE_CommonID(const char* id)
{
    SE_ASSERT(id);
    mImpl = new SE_CommonID::_Impl;
    mImpl->id = id;
}
SE_CommonID::SE_CommonID(const char* id, int size)
{
    SE_ASSERT(id);
    mImpl = new SE_CommonID::_Impl;
    mImpl->id.assign(id, size);
}
SE_CommonID::SE_CommonID(const SE_CommonID& id)
{
    SE_CommonID::_Impl* mImpl = new SE_CommonID::_Impl;
    if(!mImpl)
        return ;
    mImpl->id = id.id;
}
SE_CommonID& SE_CommonID::operator=(const SE_CommonID& id)
{
    if(this == &id)
        return *this;
    SE_CommonID::_Impl* tmp = new SE_CommonID::_Impl;
    if(!tmp)
        return *this;
    tmp->id = id.id;
    if(mImpl)
        delete mImpl;
    mImpl = tmp;
    return *this;
}
bool operator==(const SE_CommonID& id1, const SE_CommonID& id2)
{
    if(id1.mImpl->id == id2.mImpl->id)
        return true;
    else
        return false;
}
bool operator<(const SE_CommonID& id1, const SE_CommonID& id2)
{
    if(id1.mImpl->id < id2.mImpl->id)
        return true;
    else
        return false;

}
bool operator>(const SE_CommonID& id1, const SE_CommonID& id2)
{
    if(id1.mImpl->id > id2.mImpl->id)
        return true;
    else
        return false;

}

