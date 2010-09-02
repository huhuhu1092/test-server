#include "SE_SimObjectState.h"
SE_SimObjectState::SE_SimObjectState()
{}
SE_SimObjectState::~SE_SimObjectState()
{
    PropertyMap::iterator it;
    for(it = mPropertyMap.begin() ; it != mPropertyMap.end(); it++)
    {
        if(it->type == DATA && it->prop.data)
        {
            delete it->prop.data;
        }
    }
}
char SE_SimObjectState::getChar(const char* name, char defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.c;
    
}
unsigned char SE_SimObjectState::getUChar(const char* name, unsigned char defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.uc;
}
short SE_SimObjectState::getShort(const char* name, short defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
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
        return it->second.prop.s;

}
const short* SE_SimObjectState::getUniString(const char* name, const short* defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.us;

}
SE_Data* SE_SimObjectState::getData(const char* name, SE_Data* defaultValue)
{
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
        return defaultValue;
    else
        return it->second.prop.data;

}
void SE_SimObjectState::setChar(const char* name, char c)
{
    _Property p;
    p.type = CHAR;
    p.prop.c = c;
    mPropertyMap[name] = p;
}
void SE_SimObjectState::setUChar(const char* name, unsigned char uc)
{
    _Property p;
    p.type = UCHAR;
    p.prop.uc = uc;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setShort(const char* name, short s)
{
    _Property p;
    p.type = SHORT;
    p.prop.s = s;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setUShort(const char* name, unsigned short us)
{
    _Property p;
    p.type = USHORT;
    p.prop.us = us;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setInt(const char* name, int i)
{
    _Property p;
    p.type = INT;
    p.prop.i = i;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setUInt(const char* name, unsigned int ui)
{
    _Property p;
    p.type = UINT;
    p.prop.ui = ui;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setFloat(const char* name, float f)
{
    _Property p;
    p.type = FLOAT;
    p.prop.f = f;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setString(const char* name, const char* s)
{
    _Property p;
    p.type = STRING;
    p.prop.s = s;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setUniString(const char* name, const short* s16)
{
    _Property p;
    p.type = USTRING;
    p.prop.us = s16;
    mPropertyMap[name] = p;

}
void SE_SimObjectState::setData(const char* name, SE_Data* data)
{
    _Property p;
    p.type = DATA;
    p.prop.data = data;
    PropertyMap::iterator it = mPropertyMap.find(name);
    if(it == mPropertyMap.end())
    {
        mPropertyMap[name] = p;
    }
    else
    {
        if(it->second.prop.data)
            delete it->second.prop.data;
        it->second.prop.data = data;
    }

}

