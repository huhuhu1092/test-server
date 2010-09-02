#include "SE_SimObjectState.h"
SE_SimObjectState::SE_SimObjectState()
{}
SE_SimObjectState::~SE_SimObjectState()
{
    PropertyMap::iterator it;
    for(it = mPropertyMap.begin() ; it != mPropertyMap.end(); it++)
    {
        clearContent(it);
    }
}
char SE_SimObjectState::getChar(const char* name, char defaultValue)
{
    std::string str(name);
    PropertyMap::iterator it = mPropertyMap.find(str);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.c;
    
}
unsigned char SE_SimObjectState::getUChar(const char* name, unsigned char defaultValue)
{
    std::string str(name);
    PropertyMap::iterator it = mPropertyMap.find(str);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.uc;
}
short SE_SimObjectState::getShort(const char* name, short defaultValue)
{
    std::string str(name);
    PropertyMap::iterator it = mPropertyMap.find(str);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.s;

}
unsigned short SE_SimObjectState::getUShort(const char* name, unsigned short defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.us;

}
int SE_SimObjectState::getInt(const char* name, int defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.i;

}
unsigned int SE_SimObjectState::getUInt(const char* name, unsigned int defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.ui;

}
float SE_SimObjectState::getFloat(const char* name, float defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.f;

}
const char* SE_SimObjectState::getString(const char* name, const char* defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.str8;

}
const short* SE_SimObjectState::getUniString(const char* name, const short* defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.str16;

}
SE_Data* SE_SimObjectState::getData(const char* name, SE_Data* defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.data;

}
void SE_SimObjectState::clearContent(PropertyMap::iterator& it)
{
    switch(it->second.type)
    {
    case DATA:
        if(it->second.prop.data)
            it->second.prop.data;
        break;
    case STRING:
        if(it->second.prop.str8)
            delete[] it->second.prop.str8;
        break;
    case UNISTRING:
        if(it->second.prop.str16)
            delete[] it->second.prop.str16;
        break;
    }

}
void SE_SimObjectState::setContent(const char* name, const _Property& p)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
    {
        mPropertyMap[name] = p;
        return;
    }
    clearContent(it);
    it->second.type = p.type;
    it->second.prop = p.prop;
}

void SE_SimObjectState::setChar(const char* name, char c)
{
    _Property p;
    p.type = CHAR;
    p.prop.c = c;
    setContent(name, p);
}
void SE_SimObjectState::setUChar(const char* name, unsigned char uc)
{
    _Property p;
    p.type = UCHAR;
    p.prop.uc = uc;
    setContent(name, p);

}
void SE_SimObjectState::setShort(const char* name, short s)
{
    _Property p;
    p.type = SHORT;
    p.prop.s = s;
    setContent(name, p);


}
void SE_SimObjectState::setUShort(const char* name, unsigned short us)
{
    _Property p;
    p.type = USHORT;
    p.prop.us = us;
    setContent(name, p);


}
void SE_SimObjectState::setInt(const char* name, int i)
{
    _Property p;
    p.type = INT;
    p.prop.i = i;
    setContent(name, p);

}
void SE_SimObjectState::setUInt(const char* name, unsigned int ui)
{
    _Property p;
    p.type = UINT;
    p.prop.ui = ui;
    setContent(name, p);

}
void SE_SimObjectState::setFloat(const char* name, float f)
{
    _Property p;
    p.type = FLOAT;
    p.prop.f = f;
    setContent(name, p);

}
void SE_SimObjectState::setString(const char* name, const char* s, int len)
{
    _Property p;
    p.type = STRING;
    int copyLen = 0;
    if(len == 0)
    {
        copyLen = strlen(s);
    }
    else
    {
        copyLen = len;
    }
    char* tmp = new char[copyLen + 1];
    if(tmp)
    {
        strncpy(tmp, s, copyLen);
        tmp[copyLen] = '\0';
        p.prop.str8 = tmp;
        setContent(name, p);
    }

}
void SE_SimObjectState::setUniString(const char* name, const short* s16, int len)
{
    _Property p;
    p.type = UNISTRING;
    short* tmp = new short[len];
    if(tmp)
    {
        memcpy(tmp, s16, sizeof(short) * len);
        p.prop.str16 = tmp;
        setContent(name, p);
    }

}
void SE_SimObjectState::setData(const char* name, SE_Data* data)
{
    _Property p;
    p.type = DATA;
    p.prop.data = data;
    setContent(name, p);
}

