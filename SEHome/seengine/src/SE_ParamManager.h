#ifndef SE_PARAMMANAGER_H
#define SE_PARAMMANAGER_H
#include "SE_ObjectManager.h"
#include "SE_ID.h"
#include "SE_Value.h"
#include <string>
//param is defined in struct, struct is contained by xml
//for example: use param.xml/structid/paramid to get a param
// param.xml/structid/paramid is the address of the value
class SE_ParamManager
{
public:
    //enum PARAM_TYPE {INVALID, INT, FLOAT, STRING, TYPE_NUM};
    static const SE_StringID ANY_ADDRESS;
	SE_ParamManager();
	~SE_ParamManager();
	int getInt(const SE_StringID& address, bool& ok) const;
	float getFloat(const SE_StringID& address, bool& ok) const;
	std::string getString(const SE_StringID& address, bool& ok) const;
	void setInt(const SE_StringID& address, int v);
    void setFloat(const SE_StringID& address, float v);
	void setString(const SE_StringID& address, const std::string& v);
	bool hasAddress(const SE_StringID& address) const;
    // add paramObserver to the address observer list
    // if paramObserver has been added , paramObserver will not been added
    void registerObserver(const SE_StringID& address, SE_ParamObserver* paramObserver);
    // if address is null string and paramObserver is not NULL, it will unregister this paramObserver in all address
    // if address is not null string and paramObserver is NULL, it will unregister all paramObserver in this address
    // if address is null string and paramObserver is NULL, it will unregister all paramObserver in all address
    // if address is not null string and paramObserver is not NULL, it will unregister this paramObserver in this address 
    void unregisterObserver(const SE_StringID& address, SE_ParamObserver* paramObserver);
    // if address is ANYADDRESS it will update all address
    // if address is a specific address it will update this address
    void update(const SE_StringID& address);
private:
	void load(const std::string& id) const;
private:
	SE_ParamManager(const SE_ParamManager&);
	SE_ParamManager& operator=(const SE_ParamManager);
private:
	struct _Param
	{
        PARAM_TYPE type;
        SE_Value data;
        std::list<SE_ParamObserver*> observers
	};
    SE_ObjectManager<SE_StringID, _Param*> mDataMap;
};
#endif
