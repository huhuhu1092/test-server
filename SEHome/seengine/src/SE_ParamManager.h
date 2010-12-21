#ifndef SE_PARAMMANAGER_H
#define SE_PARAMMANAGER_H
#include "SE_ObjectManager.h"
#include "SE_ID.h"
#include <string>
//param is defined in struct, struct is contained by xml
//for example: use param.xml/structid/paramid to get a param
// param.xml/structid/paramid is the address of the value
class SE_ParamType
{
public:
	enum {INVALID, INT, FLOAT, STRING, TYPE_NUM};
	int type;
	SE_ParamType()
	{
		type = INVALID;
	}
};
class SE_ParamManager
{
public:
	SE_ParamManager();
	~SE_ParamManager();
	int getInt(const SE_StringID& address, bool& ok) const;
	float getFloat(const SE_StringID& address, bool& ok) const;
	std::string getString(const SE_StringID& address, bool& ok) const;
	void setInt(const SE_StringID& address, int v);
    void setFloat(const SE_StringID& address, float v);
	void setString(const SE_StringID& address, const std::string& v);
	bool hasAddress(const SE_StringID& address) const;
private:
	SE_ParamManager(const SE_ParamManager&);
	SE_ParamManager& operator=(const SE_ParamManager);
private:
	struct _Param
	{
		SE_ParamType paramType;
		union
		{
			int i;
			float f;
			std::string* str;
		};
		~_Param()
		{
			if(paramType.type == SE_ParamType::STRING)
			{
				if(str)
					delete str;
			}
		}
	};
    SE_ObjectManager<SE_StringID, _Param> mDataMap;
};
#endif