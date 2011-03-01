#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_Buffer.h"
#include "SE_Application.h"
#include "SE_CharCodeDefine.h"
#include "SE_Utils.h"
#include "SE_Log.h"
#include <stdio.h>
#include <stdlib.h>
///////////////////////
SE_StringID SE_StringID::INVALID("");
SE_StringID::~SE_StringID()
{
}
SE_StringID::SE_StringID()
{
    mType = SE_ASCII;
}
const char* SE_StringID::getStr() const
{
    return mStr.c_str();
}
SE_StringID::SE_StringID(const char* id, ENCODING_TYPE t) : mType(SE_ASCII)
{
    if(!id)
		return;
	mStr = id;
    mType = t;
}
SE_StringID::SE_StringID(const char* id, int size, ENCODING_TYPE t) : mType(SE_ASCII)
{
    if(!id)
		return;
    mStr.assign(id, size);
}

bool SE_StringID::isValid() const
{
    SE_StringID invalid("");
    return *this != invalid;
}
void SE_StringID::print() const
{
	LOGI("%s\n", mStr.c_str());
}
SE_StringID& SE_StringID::read(SE_BufferInput& input)
{
	int type = input.readInt();
    std::string str = input.readString();
    mStr = str;
	mType = (ENCODING_TYPE)type;
    return *this;
}
void SE_StringID::write(SE_BufferOutput& output)
{
	output.writeInt(mType);
    output.writeString(mStr.c_str());
}
void SE_StringID::convertToCharCode()
{
	switch(mType)
	{
	case SE_ASCII:
		{
            mCharCodeArray.resize(mStr.size());
			for(size_t i = 0 ; i < mStr.size() ; i++)
			{
				mCharCodeArray[i].set(SE_CharCodeDefine::asciiToUnicode[mStr[i]]);
			}
		}
		break;
	case SE_UNICODE:
		{
			size_t num = mStr.size() / 2;
			mCharCodeArray.resize(num);
			size_t j = 0;
			for(size_t i = 0 ; i < mStr.size() ; i += 2)
			{
                int c1 = mStr[i];
				int c2 = mStr[i + 1];
				int c = (c2 << 8) | c1;
				SE_CharCode cc((wchar_t)c);
                mCharCodeArray[j++] = cc;
			}
		}
		break;
	case SE_UTF8:
		{
			int size = SE_Util::getUtf32LenFromUtf8(mStr.c_str(), mStr.size());
			unsigned int* data = new unsigned int[size];
			SE_Util::utf8ToUtf32(mStr.c_str(), mStr.size(), data, size);
			mCharCodeArray.resize(size);
			for(int i = 0 ; i < size ; i++)
			{
				mCharCodeArray[i].set(data[i]);
			}
		}
		break;
	case SE_UTF16:
		{
		}
		break;
	case SE_UTF32:
		{
		}
		break;
	}
}
int SE_StringID::getCharNum()
{
    int num = 0;
	convertToCharCode();
    return mCharCodeArray.size();

}
SE_CharCode SE_StringID::getCharCode(int i)
{
	if(i < 0 || i >= mCharCodeArray.size())
		return SE_CharCode();
	return mCharCodeArray[i];
}
bool operator==(const SE_StringID& id1, const SE_StringID& id2)
{
    if(id1.mStr == id2.mStr)
        return true;
    else
        return false;
}
bool operator!=(const SE_StringID& id1, const SE_StringID& id2)
{
	return !(id1 == id2);
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
    return *this != invalid;

}
void SE_CommonID::print() const
{
    LOGI("%d, %d, %d, %d\n", id[0], id[1], id[2], id[3]);
}
std::string SE_CommonID::toString() const
{
    char buf[41];
    memset(buf, 0, 41);
#if defined(WIN32)
    _snprintf(buf, 40, "%d%d%d%d", id[0], id[1], id[2], id[3]);
#else
    snprintf(buf, 40, "%d%d%d%d", id[0], id[1], id[2], id[3]);
#endif
    std::string str = buf;
    return str;
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
//////////////////////////////////////////
SE_TreeStructID SE_TreeStructID::INVALID = SE_TreeStructID(0xFFFFFFFE, 0xFFFFFFFE);
SE_TreeStructID SE_TreeStructID::NULLID = SE_TreeStructID(0xFFFFFFFF, 0xFFFFFFFF);
std::string SE_TreeStructID::toString() const
{
    char buf[21];
    memset(buf, 0, 21);
#if defined(WIN32)
    _snprintf(buf, 20, "%d%d", id[0], id[1]);
#else
    snprintf(buf, 20, "%d%d", id[0], id[1]);
#endif
    std::string str = buf;
    return str;
}
void SE_TreeStructID::write(SE_BufferOutput& output)
{
	output.writeInt(id[0]);
	output.writeInt(id[1]);
}
void SE_TreeStructID::read(SE_BufferInput& input)
{
	id[0] = input.readInt();
	id[1] = input.readInt();
}
////////////////////////////////////////

SE_ImageDataID SE_ID::createImageDataID(const char* str)
{
	return SE_StringID(str);
}
SE_MeshID SE_ID::createMeshID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();
}
/*
SE_SpatialID SE_ID::createSpatialID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
*/
SE_GeometryDataID SE_ID::createGeometryDataID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
SE_TextureCoordDataID SE_ID::createTextureCoordDataID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();
}
SE_MaterialDataID SE_ID::createMaterialDataID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
/*
SE_SceneID SE_ID::createSceneID(const char* str)
{
    return SE_StringID(str);
}
*/
SE_ProgramDataID SE_ID::createProgramDataID(const char* str)
{
    return SE_StringID(str);

}
SE_CommandID SE_ID::createCommandID(const char* str)
{
    return SE_StringID(str);

}
SE_CommandFactoryID SE_ID::createCommandFactoryID(const char* str)
{
    return SE_StringID(str);

}
SE_PrimitiveID SE_ID::createPrimitiveID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
/*
SE_AnimationID SE_ID::createAnimationID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
*/
/*
SE_SimObjectID SE_ID::createSimObjectID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
*/
/*
SE_ElementID SE_ID::createElementID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
*/
SE_SkinJointControllerID SE_ID::createSkinJointControllerID(const char* str)
{
    return SE_StringID(str);
}
SE_MountPointID SE_ID::createMountPointID(const char* str)
{
    return SE_StringID(str);

}
SE_RendererID SE_ID::createRendererID(const char* str)
{
    return SE_StringID(str);

}
/*
SE_CameraID SE_ID::createCameraID(const char* str)
{
    return SE_Application::getInstance()->createCommonID();

}
*/
