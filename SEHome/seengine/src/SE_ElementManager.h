#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include "SE_TreeStructManager.h"
class SE_ElementManager
{
public:
    enum {MAX_SIZE = 20000};
    enum {SIZE = 1000};
    SE_ElementID addElement(const SE_ElementID& parent, SE_Element* e);
    SE_Element* findElement(const SE_ElementID& id);
    SE_Element* removeElement(const SE_ElementID& id);
    SE_Element* getParent(const SE_ElementID& id);
    std::vector<SE_Element*> getChildren(const SE_ElementID& id) const;
    void releaseElement(SE_Element* element, int delay = SE_TreeStructManager<SE_Element>::RELEASE_DELAY);
    void releaseElement(const SE_ElementID& id, int delay = SE_TreeStructManager<SE_Element>::RELEASE_DELAY);
private:
    SE_TreeStructManager<SE_Element> mElements;
};
#endif
