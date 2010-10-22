#ifndef SE_SEQUENCE_H
#define SE_SEQUENCE_H
#include "SE_TableManager.h"
#include "SE_ID.h"
struct SE_SequenceFrame
{
    unsigned int key;
    SE_StringID unitID;
};
class SE_Sequence
{
public:
    SE_StringID getImageMapRef()
    {
        return mImageMapRef;
    }
    void setImageMapRef(const SE_StringID& ref)
    {
        mImageMapRef = ref;
    }
    void setItem(unsigned int key, const SE_StringID& unitID)
    {}
    SE_StringID& getItem(unsigned int key)
    {
        
    }
private:
    SE_StringID mImageMapRef;
    std::list<>
};
#endif
