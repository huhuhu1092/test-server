#include "SE_ParamManager.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Math.h"
#include <algorithm>
#include <stdio.h>
const SE_AddressID SE_ParamManager::ANY_ADDRESS = "*#*#08*#*#";
SE_ParamManager::SE_ParamManager()
{}
SE_ParamManager::~SE_ParamManager()
{}
void SE_ParamManager::load(const std::string& id) const
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->loadParam(id.c_str());
}
int SE_ParamManager::getInt(const SE_AddressID& address, bool& ok) const 
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
	_Param* retv = mDataMap.get(address);
	if(!retv || (retv->data.getType() != SE_Value::INT_T))
	{
		ok = false;
		return 0x7fffffff;
	}
	else
	{
		ok = true;
		return retv->data.getInt();
	}
}
float SE_ParamManager::getFloat(const SE_AddressID& address, bool& ok) const
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
	_Param* retv = mDataMap.get(address);
	if(!retv || (retv->data.getType() != SE_Value::FLOAT_T))
	{
		ok = false;
		return 0.0f;
	}
	else
	{
		ok = true;
		return retv->data.getFloat();
	}
}
std::string SE_ParamManager::getString(const SE_AddressID& address, bool& ok) const
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
    _Param* retv = mDataMap.get(address);
	if(!retv || (retv->data.getType() != SE_Value::ASCII_T))
	{
		if(retv->data.getType() == SE_Value::INT_T)
		{ 
			int v = retv->data.getInt();
			char buf[10];
			memset(buf, 0, 10);
#if defined(WIN32)
		    _snprintf(buf, 9, "%d", v);
#else
			snprintf(buf, 9, "%d", v);
#endif
			std::string str(buf);
			ok = true;
			return str;
		}
		else
		{
		    ok = false;
		    return "";
		}
	}
	else
	{
		ok = true;
		return retv->data.getAscii();
	}
}
void SE_ParamManager::setInt(const SE_AddressID& address, int v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param;
	    mDataMap.set(address, p);
	}
	p->data.setInt(v);
	if(bUpdate)
		update(address, p);
}
void SE_ParamManager::setFloat(const SE_AddressID& address, float v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param;
	    mDataMap.set(address, p);
	}
	p->data.setFloat(v);
	if(bUpdate)
		update(address, p);
}
void SE_ParamManager::update(const SE_AddressID& address, const _Param* param)
{
	_ObserverList::const_iterator it;
	for(it = param->observers.begin() ; it != param->observers.end() ; it++)
	{
		SE_ParamObserver* po = *it;
		po->update(address, param->data);
	}
}
void SE_ParamManager::setString(const SE_AddressID& address, const std::string& v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param;
	    mDataMap.set(address, p);
	}
	p->data.setAscii(v.c_str());
	if(bUpdate)
		update(address, p);
}
bool SE_ParamManager::hasAddress(const SE_AddressID& address) const
{
	return mDataMap.isContain(address);
}
void SE_ParamManager::registerObserver(const SE_AddressID& address, SE_ParamObserver* paramObserver)
{
    _Param* p = mDataMap.get(address);
	if(p)
	{
		_ObserverList::iterator it = find(p->observers.begin(), p->observers.end(), paramObserver);
		if(it != p->observers.end())
		{
            *it = paramObserver;
		}
		else
		{
		    p->observers.push_back(paramObserver);
		}
	}
}
void SE_ParamManager::unregisterObserver(const SE_AddressID& address, SE_ParamObserver* paramObserver)
{
    if(address == ANY_ADDRESS && paramObserver == NULL)
	{
        ParamVisitor pv;
		pv.op = UNREGISTER_ALL;
		pv.paramManager = this;
		mDataMap.traverse(pv);
	}
	else if(address == ANY_ADDRESS && paramObserver != NULL)
	{
		ParamVisitor pv;
		pv.op = UNREGISTER_BY_OBSERVER;
		pv.paramManager = this;
		pv.ob = paramObserver;
		mDataMap.traverse(pv);
	}
	else if(address != ANY_ADDRESS)
	{
		ParamVisitor pv;
		pv.op = UNREGISTER_BY_ID;
		pv.id = address;
		pv.ob = paramObserver;
		pv.paramManager = this;
		mDataMap.traverse(pv);
	}
}

void SE_ParamManager::update(const SE_AddressID& address)
{
	if(address != ANY_ADDRESS)
	{
	    _Param* p = mDataMap.get(address);
	    if(p)
	    {
		    update(address, p);
	    }
	}
	else
	{
		ParamVisitor pv;
		pv.paramManager = this;
		pv.op = UPDATE;
		mDataMap.traverse(pv);
	}
}
