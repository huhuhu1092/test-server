#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include <wchar.h>
#include <string>
#include <map>
#include "SE_Element.h"
#include "SE_ElementMap.h"
#include "SE_Geometry3D.h"
class SE_Spatial;

class SE_ElementManager
{
public:
    SE_ElementManager();
    ~SE_ElementManager();
	void setViewport(int left, int top, int width ,int height);
	SE_Rect<int> getViewport()
	{
		return mViewport;
	}
    void load(const char* elementURI);
    SE_Spatial* createSpatial();
	void spawn();
	void measure();
	void update(unsigned int key);
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
	SE_Rect<int> mViewport;
    //SE_ElementMapManager mElementMapManager;
	//SE_ElementMap* mCurrElementMap;
};
#endif
