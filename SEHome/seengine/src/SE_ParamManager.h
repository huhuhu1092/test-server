#ifndef SE_PARAMMANAGER_H
#define SE_PARAMMANAGER_H
#include "SE_ObjectManager.h"
#include "SE_ID.h"
#include "SE_Value.h"
#include <string>
#include <list>
#include <vector>
#include <algorithm>
#include "SE_ParamObserver.h"
struct SE_ParamValue
{
    SE_AddressID param;
    SE_Value value;
    SE_ParamValue()
    {}
    SE_ParamValue(const SE_AddressID& p, const SE_Value& v) : param(p), value(v)
    {}
};
class SE_ParamValueList
{
public:
    void add(const SE_AddressID& param, const SE_Value& v)
    {
        SE_ParamValue pv(param, v);
        mParamValueList.push_back(pv);
    }
    void add(std::list<SE_ParamValue>& tmpList)
    {
        mParamValueList.splice(mParamValueList.end(), tmpList);
    }
    std::vector<SE_ParamValue> getParamValue()
    {
        std::vector<SE_ParamValue> retV(mParamValueList.size());
        copy(mParamValueList.begin(), mParamValueList.end(), retV.begin());
        return retV;
    }
private:
    std::list<SE_ParamValue> mParamValueList;
};
//param is defined in struct, struct is contained by xml
//for example: use param.xml/structid/paramid to get a param
// param.xml/structid/paramid is the address of the value
class SE_ParamManager
{
public:
    //enum PARAM_TYPE {INVALID, INT, FLOAT, STRING, TYPE_NUM};
    static const SE_AddressID ANY_ADDRESS;
	SE_ParamManager();
	~SE_ParamManager();
	int getInt(const SE_AddressID& address, bool& ok) const;
	float getFloat(const SE_AddressID& address, bool& ok) const;
	std::string getString(const SE_AddressID& address, bool& ok) const;
	void setInt(const SE_AddressID& address, int v, bool bUpdate = false);
    void setFloat(const SE_AddressID& address, float v, bool bUpdate = false);
	void setString(const SE_AddressID& address, const std::string& v, bool bUpdate = false);
	bool hasAddress(const SE_AddressID& address) const;
    // add paramObserver to the address observer list
    // if paramObserver has been added , paramObserver will not been added
    void registerObserver(const SE_AddressID& address, SE_ParamObserver* paramObserver);
    // if address is null string and paramObserver is not NULL, it will unregister this paramObserver in all address
    // if address is not null string and paramObserver is NULL, it will unregister all paramObserver in this address
    // if address is null string and paramObserver is NULL, it will unregister all paramObserver in all address
    // if address is not null string and paramObserver is not NULL, it will unregister this paramObserver in this address 
    void unregisterObserver(const SE_AddressID& address, SE_ParamObserver* paramObserver);
    // if address is ANYADDRESS it will update all address
    // if address is a specific address it will update this address
    void update(const SE_AddressID& address);
private:
	void load(const std::string& id) const;
private:
	SE_ParamManager(const SE_ParamManager&);
	SE_ParamManager& operator=(const SE_ParamManager);
private:
    typedef std::list<SE_ParamObserver*> _ObserverList;
	struct _Param
	{
        SE_Value data;
		mutable _ObserverList observers;
	};
	void update(const SE_AddressID& address, const _Param* param);
	enum {UPDATE, UNREGISTER_ALL, UNREGISTER_BY_ID, UNREGISTER_BY_OBSERVER};
	class ParamVisitor : public SE_ObjectManagerVisitor<SE_AddressID, _Param*>
	{
	public:
		void visit(const SE_AddressID& id, const _Param* param)
		{
			if(op == UPDATE)
			{
			    paramManager->update(id, param);
			}
			else if(op == UNREGISTER_ALL)
			{
				param->observers.clear();
			}
			else if(op == UNREGISTER_BY_ID)
			{
				if(id == this->id)
				{
					if(ob == NULL)
					{
						param->observers.clear();
					}
					else
					{
                    #if defined(WIN32)
						_ObserverList::const_iterator it;
                    #else
                        _ObserverList::iterator it;
                    #endif
						for(it = param->observers.begin() ; it != param->observers.end() ; it++)
						{
							SE_ParamObserver* po = *it;
							if(po == ob)
							{
								break;
							}
						}
						if(it != param->observers.end())
						    param->observers.erase(it);
					}
				}
			}
			else if(op == UNREGISTER_BY_OBSERVER)
			{
            #if defined(WIN32)
				_ObserverList::const_iterator it;
            #else
                _ObserverList::iterator it;
            #endif
				for(it = param->observers.begin() ; it != param->observers.end() ; it++)
				{
                    SE_ParamObserver* po = *it;
					if(po == ob)
						break;
				}
				if(it != param->observers.end())
					param->observers.erase(it);
			}
		}
        SE_ParamManager* paramManager;    
		int op;
		SE_AddressID id;
		SE_ParamObserver* ob;
	};

private:
    SE_ObjectManager<SE_AddressID, _Param*> mDataMap;
};
#endif
