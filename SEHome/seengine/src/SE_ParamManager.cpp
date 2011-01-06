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
	_Param* retv = mDataMap.get(address);
    if(retv == NULL)
    {
        ok = false;
        return 0x7ffffffff;
    }
	if(retv->data.type != SE_Value::INT)
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
float SE_ParamManager::getFloat(const SE_StringID& address, bool& ok) const
{
	SE_Util::SplitStringList strList = SE_Util::splitString(address.getStr(), "/");
	if(!mDataMap.isContain(strList[0].c_str()))
	{
		load(strList[0]);
	}
	_Param* retv = mDataMap.get(address);
	if(!rev || retv->data.getType() != SE_Value::FLOAT)
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
	if(!retv || retv->data.getType() != SE_Value::ASCII)
	{
		ok = false;
		return "";
	}
	else
	{
		ok = true;
		return retv->getAscii();
	}
}
void SE_ParamManager::setInt(const SE_StringID& address, int v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param;
	}
	p->data.setInt(v);
	mDataMap.set(address, p);
	if(bUpdate)
		update(address, p);
}
void SE_ParamManager::setFloat(const SE_StringID& address, float v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param;
	}
	p->data.setFloat(v);
	mDataMap.set(address, p);
	if(bUpdate)
		update(address, p);
}
void SE_ParamManager::update(const SE_StringID& address, const _Param* param)
{
	_ObserverList::const_iterator it;
	for(it = observers.begin() ; it != observers.end() ; it++)
	{
		SE_ParamObserver* po = *it;
		po->update(address, p->data);
	}
}
void SE_ParamManager::setString(const SE_StringID& address, const std::string& v, bool bUpdate)
{
	_Param* p = mDataMap.get(address);
	if(!p)
	{
	    p = new _Param; 
	}
	p->data.setAscii(v.c_str());
	mDataMap.set(address, p);
	if(bUpdate)
		updateObserver(address, p);
}
bool SE_ParamManager::hasAddress(const SE_StringID& address) const
{
	return mDataMap.isContain(address);
}
void SE_ParamManager::registerObserver(const SE_StringID& address, SE_ParamObserver* paramObserver)
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
void SE_ParamManager::unregisterObserver(const SE_StringID& address, SE_ParamObserver* paramObserver)
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
		pv.ob = paramObserver
		pv.paramManager = this;
		mDataMap.traverse(pv);
	}
}

void SE_ParamManager::update(const SE_StringID& address)
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
