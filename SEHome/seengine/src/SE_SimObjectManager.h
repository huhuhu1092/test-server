#ifndef SE_SIMOBJECTMANAGER_H
#define SE_SIMOBJECTMANAGER_H
#include "SE_ID.h"
#include "SE_SimObject.h"
//#include "SE_ObjectManager.h"
#include "SE_TreeStructManager.h"
class SE_SimObjectManager
{
public:
    SE_SimObjectID add(SE_SimObject* simObject);
    SE_SimObject* get(const SE_SimObjectID& simObjectID);
    SE_SimObject* remove(const SE_SimObjectID& simObjectiD);
	std::vector<SE_SimObject*> find(const char* name);
	void release(const SE_SimObjectID& id, int delay = SE_RELEASE_DELAY);
	void release(SE_SimObject* simObject, int delay = SE_RELEASE_DELAY);
private:
    //SE_ObjectManager<SE_SimObjectID, SE_SimObject*> mSimObjectManager;
	SE_TreeStructManager<SE_SimObject> mSimObjectManager;
};
#endif
