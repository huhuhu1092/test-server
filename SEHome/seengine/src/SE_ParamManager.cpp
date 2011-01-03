#include "SE_ParamManager.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Math.h"
SE_ParamManager::SE_ParamManager()
{}
SE_ParamManager::~SE_ParamManager()
{}
void SE_ParamManager::load(const std::string& id) const
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadParam(id.c_str());
}
int SE_ParamManager::getInt(const SE_StringID& address, bool& ok) const 
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
	_Param retv = mDataMap.get(address);
	if(retv.paramType.type != SE_ParamType::INT)
	{
		ok = false;
		return 0x7fffffff;
	}
	else
	{
		ok = true;
		return retv.i;
	}
}
float SE_ParamManager::getFloat(const SE_StringID& address, bool& ok) const
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
	_Param retv = mDataMap.get(address);
	if(retv.paramType.type != SE_ParamType::FLOAT)
	{
		ok = false;
		return 0.0f;
	}
	else
	{
		ok = true;
		return retv.f;
	}
}
std::string SE_ParamManager::getString(const SE_StringID& address, bool& ok) const
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
    _Param retv = mDataMap.get(address);
	if(retv.paramType.type != SE_ParamType::STRING)
	{
		ok = false;
		return "";
	}
	else
	{
		ok = true;
		return *retv.str;
	}
}
void SE_ParamManager::setInt(const SE_StringID& address, int v)
{
	_Param p;
	p.paramType.type = SE_ParamType::INT;
	p.i = v;
	mDataMap.set(address, p);
}
void SE_ParamManager::setFloat(const SE_StringID& address, float v)
{
	_Param p;
	p.paramType.type = SE_ParamType::FLOAT;
	p.f = v;
	mDataMap.set(address, p);
}
void SE_ParamManager::setString(const SE_StringID& address, const std::string& v)
{
	_Param p;
	p.paramType.type = SE_ParamType::STRING;
	p.str = new std::string(v);
	mDataMap.set(address, p);
}
bool SE_ParamManager::hasAddress(const SE_StringID& address) const
{
	return mDataMap.isContain(address);
}