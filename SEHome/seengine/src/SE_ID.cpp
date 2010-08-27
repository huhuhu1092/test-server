#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_Buffer.h"
#include "SE_Log.h"

///////////////////////
SE_StringID SE_StringID::INVALID("");
/*
struct SE_StringID::_Impl
{
    std::string id;
};
*/
SE_StringID::~SE_StringID()
{
}
SE_StringID::SE_StringID()
{
    //mImpl = new SE_StringID::_Impl;
}
const char* SE_StringID::getStr() const
{
    return mStr.c_str();
}
SE_StringID::SE_StringID(const char* id)
{
    SE_ASSERT(id);
    //mImpl = new SE_StringID::_Impl;
    //mImpl->id = id;
	mStr = id;
}
SE_StringID::SE_StringID(const char* id, int size)
{
    SE_ASSERT(id);
    //mImpl = new SE_StringID::_Impl;
    mStr.assign(id, size);
}
/*
SE_StringID::SE_StringID(const SE_StringID& id)
{
    mImpl = new SE_StringID::_Impl;
    if(!mImpl)
        return ;
    mImpl->id = id.mImpl->id;

}
*/
/*
SE_StringID& SE_StringID::operator=(const SE_StringID& id)
{
    if(this == &id)
        return *this;
    SE_StringID::_Impl* tmp = new SE_StringID::_Impl;
    if(!tmp)
        return *this;
    tmp->id = id.mImpl->id;
    if(mImpl)
        delete mImpl;
    mImpl = tmp;
    return *this;
}
*/
bool SE_StringID::isValid() const
{
    SE_StringID invalid("");
    return *this == invalid;
}
void SE_StringID::print() const
{
	LOGI("%s\n", mStr.c_str());
}
SE_StringID& SE_StringID::read(SE_BufferInput& input)
{
    std::string str = input.readString();
    mStr = str;
    return *this;
}
void SE_StringID::write(SE_BufferOutput& output)
{
    output.writeString(mStr.c_str());
}
bool operator==(const SE_StringID& id1, const SE_StringID& id2)
{
    if(id1.mStr == id2.mStr)
        return true;
    else
        return false;
}
bool operator<(const SE_StringID& id1, const SE_StringID& id2)
{
    if(id1.mStr < id2.mStr)
        return true;
    else
        return false;

}
bool operator>(const SE_StringID& id1, const SE_StringID& id2)
{
    if(id1.mStr > id2.mStr)
        return true;
    else
        return false;

}
//////////////////////////////////////////////
SE_CommonID SE_CommonID::INVALID(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);

SE_CommonID::SE_CommonID()
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = 0xFFFFFFFF;
    }
}
SE_CommonID::SE_CommonID(int i0, int i1, int i2, int i3)
{
    id[0] = i0;
    id[1] = i1;
    id[2] = i2;
    id[3] = i3;
}
SE_CommonID::SE_CommonID(const SE_CommonID& rid)
{
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
}
SE_CommonID& SE_CommonID::operator=(const SE_CommonID& rid)
{
    if(this == &rid)
        return *this;
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
    return *this;
}
void SE_CommonID::write(SE_BufferOutput& output)
{
    output.writeIntArray((int*)id, 4);

}
SE_CommonID& SE_CommonID::read(SE_BufferInput& input)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = input.readInt();
    }
    return *this;
}
bool SE_CommonID::isValid()
{
    SE_CommonID invalid(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
    return *this == invalid;

}
void SE_CommonID::print() const
{
    LOGI("%d, %d, %d, %d\n", id[0], id[1], id[2], id[3]);
}
bool operator==(const SE_CommonID& lid, const SE_CommonID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) == 0;
}
bool operator<(const SE_CommonID& lid, const SE_CommonID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) < 0;
}
bool operator>(const SE_CommonID& lid, const SE_CommonID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) > 0;
}
bool operator!=(const SE_CommonID& lid, const SE_CommonID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) != 0;

}

////////////////////////////////////////////
/*
SE_GeometryDataID::SE_GeometryDataID()
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = 0xFFFFFFFF;
    }
}
SE_GeometryDataID::SE_GeometryDataID(int i0, int i1, int i2, int i3)
{
    id[0] = i0;
    id[1] = i1;
    id[2] = i2;
    id[3] = i3;
}
SE_GeometryDataID::SE_GeometryDataID(const SE_GeometryDataID& rid)
{
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
}
SE_GeometryDataID& SE_GeometryDataID::operator=(const SE_GeometryDataID& rid)
{
    if(this == &rid)
        return *this;
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
    return *this;
}
bool SE_GeometryDataID::isValid()
{
    SE_GeometryDataID invalid(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
    return *this == invalid;
}
void SE_GeometryDataID::write(SE_BufferOutput& output)
{
    output.writeIntArray((int*)id, 4);
}
SE_GeometryDataID& SE_GeometryDataID::read(SE_BufferInput& input)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = input.readInt();
    }
    return *this;
}
bool operator==(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) == 0;
}
bool operator<(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) < 0;
}
bool operator>(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) > 0;
}
bool operator!=(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) != 0;
}
/////////////////////////////////////////////////////////////////////
SE_TextureCoordDataID::SE_TextureCoordDataID()
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = 0xFFFFFFFF;
    }

}
SE_TextureCoordDataID::SE_TextureCoordDataID(int i0, int i1, int i2, int i3)
{
    id[0] = i0;
    id[1] = i1;
    id[2] = i2;
    id[3] = i3;
}

SE_TextureCoordDataID::SE_TextureCoordDataID(const SE_TextureCoordDataID& rid)
{
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
}
SE_TextureCoordDataID& SE_TextureCoordDataID::operator=(const SE_TextureCoordDataID& rid)
{
    if(this == &rid)
        return *this;
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
    return *this;
}
void SE_TextureCoordDataID::write(SE_BufferOutput& output)
{
    output.writeIntArray((int*)id, 4);
}
bool SE_TextureCoordDataID::isValid()
{
    SE_TextureCoordDataID invalid(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
    return *this == invalid;
}
SE_TextureCoordDataID& SE_TextureCoordDataID::read(SE_BufferInput& input)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = input.readInt();
    }
    return *this;
}
bool operator==(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid)
{
return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) == 0;
}
bool operator<(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) < 0;
}
bool operator>(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) > 0;
}
bool operator!=(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) != 0;
}
///////////////////////
SE_MaterialDataID::SE_MaterialDataID()
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = 0xFFFFFFFF;
    }

}
bool SE_MaterialDataID::isValid()
{
    SE_MaterialDataID invalid(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
    return *this == invalid;
}
SE_MaterialDataID::SE_MaterialDataID(int i0, int i1, int i2, int i3)
{
    id[0] = i0;
    id[1] = i1;
    id[2] = i2;
    id[3] = i3;
}

SE_MaterialDataID::SE_MaterialDataID(const SE_MaterialDataID& rid)
{
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
}
SE_MaterialDataID& SE_MaterialDataID::operator=(const SE_MaterialDataID& rid)
{
    if(this == &rid)
        return *this;
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
    return *this;
}
void SE_MaterialDataID::write(SE_BufferOutput& output)
{
    output.writeIntArray((int*)id, 4);
}
SE_MaterialDataID& SE_MaterialDataID::read(SE_BufferInput& input)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = input.readInt();
    }
    return *this;
}
bool operator==(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) == 0;
}
bool operator<(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) < 0;
}
bool operator>(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) > 0;
}
bool operator!=(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) != 0;
}
////////////////////
SE_SceneID::SE_SceneID(int i0, int i1, int i2, int i3)
{
    id[0] = i0;
    id[1] = i1;
    id[2] = i2;
    id[3] = i3;
}
SE_SceneID::SE_SceneID(const SE_SceneID& rid)
{
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
}
SE_SceneID& SE_SceneID::operator=(const SE_SceneID& rid)
{
    if(this == &rid)
        return *this;
    memcpy(id, rid.id, sizeof(unsigned int) * 4);
    return *this;
}
bool operator==(const SE_SceneID& lid, const SE_SceneID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) == 0;
}
bool operator<(const SE_SceneID& lid, const SE_SceneID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) < 0;
}
bool operator>(const SE_SceneID& lid, const SE_SceneID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) > 0;
}
bool operator!=(const SE_SceneID& lid, const SE_SceneID& rid)
{
    return memcmp(lid.id, rid.id, sizeof(unsigned int) * 4) != 0;
}
bool SE_SceneID::isValid()
{
    SE_SceneID invalid(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF);
    return *this == invalid;
}
void SE_SceneID::write(SE_BufferOutput& output)
{
    output.writeIntArray((int*)id, 4);
}
SE_SceneID& SE_SceneID::read(SE_BufferInput& input)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        id[i] = input.readInt();
    }
    return *this;
}
*/
/////////////////////////

