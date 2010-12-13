#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include <wchar.h>
#include <string>
#include <map>
#include "tinyxml.h"
#include "SE_Element.h"
#include "SE_ElementMap.h"
class SE_Spatial;

class SE_ElementManager
{
public:
    SE_ElementManager();
    ~SE_ElementManager();
    void load(const char* elementURI);
    SE_Spatial* createSpatial();
    void addElement(SE_Element* parent, SE_Element* child);
    void removeElement(SE_Element* e);
    SE_Element* getRoot() const
    {
        return mRoot;
    }
    void setRoot(SE_Element* root)
    {
        if(mRoot)
            delete mRoot;
        mRoot = root;
    }
    SE_Element* findByID(const SE_ElementID& id);
private:
    SE_ElementManager(const SE_ElementManager&);
    SE_ElementManager& operator=(const SE_ElementManager&);
private:
    SE_Element* mRoot;
    //SE_ElementMapManager mElementMapManager;
	//SE_ElementMap* mCurrElementMap;
};
#endif
