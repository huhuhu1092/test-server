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
	~SE_CommandEntry()
	{
		delete cf;
	}
};
class SE_CommandFactory
{
public:
	enum {DEFAULT_SIZE = 100};
    SE_CommandFactory();
    SE_CommandFactory(SE_CommandEntry* entryArray[], int size);
    void set(SE_CommandEntry* entryArray[], int size);
    SE_Command* create(SE_Application* app, const SE_CommandID& id) ;
	void add(const SE_CommandID& commandID, SE_CreateCommandFunc* cf);
	void remove(const SE_CommandID& commandID);
    virtual ~SE_CommandFactory();
private:
    SE_CommandEntry** mEntryArray;
    int mEntrySize;
};
#endif
