#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include <wchar.h>
class SE_Spatial;
class SE_ElementManager
{
public:
    SE_ElementManager();
    ~SE_ElementManager();
    void load(const wchar_t* filePath);
    SE_Spatial* createSpatial();
    void addElement(SE_Element* parent, SE_Element* child);
    void removeElement(SE_Element* e);
    SE_Element* getRoot();
private:
    SE_ElementManager(const SE_ElementManager&);
    SE_ElementManager& operator=(const SE_ElementManager&);
private:
    SE_Element* mRoot;
};
#endif
