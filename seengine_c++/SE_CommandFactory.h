#ifndef SE_COMMANDFACTORY_H
#define SE_COMMANDFACTORY_H
#include "SE_Common.h"
#include "SE_Command.h"
class SE_Application;
class SE_CreateCommandFunc
{
public:
    virtual SE_Command* create(SE_Application* app, const SE_CommandID& id) = 0;
};
struct SE_CommandEntry
{
    SE_CommandID id;
    SE_CreateCommandFunc* cf;
	SE_CommandEntry()
	{
		cf = NULL;
	}
    SE_CommandEntry(const SE_CommandID& cid, SE_CreateCommandFunc* dcf): id(cid), cf(dcf)
    {
    }
};
class SE_CommandFactory
{
public:
    SE_CommandFactory(SE_CommandEntry** entryArray, int size);
    virtual SE_Command* create(SE_Application* app, const SE_CommandID& id) ;
    virtual ~SE_CommandFactory();
private:
    SE_CommandEntry** mEntryArray;
    int mEntrySize;
};
#endif
