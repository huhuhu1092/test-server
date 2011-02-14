#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include "SE_ID.h"
#include "SE_TreeStructManager.h"
#include <list>
#include <vector>
class SE_Element;
typedef int SE_ElementEventType;
class SE_ElementEvent
{
public:
    SE_ElementEvent()
    {
        mNeedMerge = false;
    }
    virtual ~SE_ElementEvent() {}
    virtual void run() = 0;
    bool isNeedMerge()
    {
        return mNeedMerge;
    }
    void setNeedMerge(bool b)
    {
        mNeedMerge = b;
    }
    virtual bool merge(SE_ElementEvent* mergeEvent)
    {
        return false;
    }
    SE_ElementEventType getType() 
    {
        return mType;
    }
protected:
    SE_ElementEventType mType; 
private:
    bool mNeedMerge;
};
class SE_ElementManager
{
public:
    enum {MAX_SIZE = 20000};
    enum {SIZE = 1000};
    SE_ElementID addElement(const SE_ElementID& parent, SE_Element* e);
    void addElement(SE_Element* parent, SE_Element* e);
    SE_Element* findElement(const SE_ElementID& id);
    SE_Element* removeElement(const SE_ElementID& id);
    SE_Element* getParent(const SE_ElementID& id);
    std::vector<SE_Element*> getChildren(const SE_ElementID& id) const;
    void releaseElement(SE_Element* element, int delay = SE_RELEASE_DELAY);
    void releaseElement(const SE_ElementID& id, int delay = SE_RELEASE_DELAY);
	void addEvent(SE_ElementEvent* event);
    void update();
private:
    SE_TreeStructManager<SE_Element> mElements;
	std::list<SE_ElementEvent*> mElemetnEventList;
};
#endif
