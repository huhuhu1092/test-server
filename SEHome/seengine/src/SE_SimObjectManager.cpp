#include "SE_SimObjectManager.h"
#include <string>
#include <list>
#include <vector>
SE_SimObjectID SE_SimObjectManager::add(SE_SimObject* simObject)
{
	return mSimObjectManager.add(SE_SimObjectID::NULLID, simObject);
}
SE_SimObject* SE_SimObjectManager::get(const SE_SimObjectID& simObjectID)
{
    return mSimObjectManager.find(simObjectID);
}
SE_SimObject* SE_SimObjectManager::remove(const SE_SimObjectID& simObjectID)
{
    return mSimObjectManager.remove(simObjectID);
}
/*
struct _FindSimObj : public SE_FindObjCondition<SE_SimObject*>
{
	bool isSatisfy(SE_SimObject* obj)
	{
		if(objname == obj->getName())
			return true;
		else
			return false;
	}
	std::string objname;
};
*/
class _FindSimObj
{
public:
	void operator()(SE_SimObject* obj)
	{
		if(name == obj->getName())
		{
			objs.push_back(obj);
		}
	}
	std::string name;
	std::list<SE_SimObject*> objs;
};
std::vector<SE_SimObject*> SE_SimObjectManager::find(const char* name)
{
	/*
	_FindSimObj fo;
	fo.objname = name;
	SE_SimObject* ret = mSimObjectManager.find(fo);
	return ret;
	*/
	_FindSimObj fo;
	fo.name = name;
	mSimObjectManager.traverseList(fo);
	std::vector<SE_SimObject*> ret(fo.objs.size());
	copy(fo.objs.begin(), fo.objs.end(), ret.begin());
	return ret;
}
