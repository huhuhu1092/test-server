#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include <wchar.h>
#include <string>
#include <list>
#include <map>
#include "SE_Element.h"
#include "SE_ElementMap.h"
#include "SE_Geometry3D.h"
class SE_Spatial;
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
    static SE_ElementEventType getType() 
    {
        return mType;
    }
protected:
    static SE_ElementEventType mType; 
private:
    bool mNeedMerge;
};
class SE_ElementManager
{
public:
	struct TextureProperty
	{
		SE_ImageDataID imageDataID;
		SE_ImageData* imageData;
	};
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
    bool addRenderTargetElement(SE_Element* child);
    void removeRenderTargetElement(SE_Element* e);
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
    void addEvent(SE_ElementEvent* event);
    void update();
private:
    SE_ElementManager(const SE_ElementManager&);
    SE_ElementManager& operator=(const SE_ElementManager&);
private:
    SE_Element* mRoot;
	SE_Rect<int> mViewport;
	std::list<SE_Element*> mRenderTargetElementList;
    std::list<SE_ElementEvent*> mElemetnEventList;
};
#endif
