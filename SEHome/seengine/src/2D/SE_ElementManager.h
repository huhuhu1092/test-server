#ifndef SE_ELEMENTMANAGER_H
#define SE_ELEMENTMANAGER_H
#include <wchar.h>
#include <string>
#include <map>
#include "tinyxml.h"
#include "SE_Element.h"
#include "SE_ElementMap.h"
class SE_Spatial;
class SE_ElementManager;
/////////////////////////////////////////////////////////////
/*
class SE_AnimationHandler : public SE_XmlElementHandler
{
public:
    SE_AnimationHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_TextureCoordAnimationHandler : public SE_XmlElementHandler
{
public:
    SE_TextureCoordAnimationHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageDataHandler : public SE_XmlElementHandler
{
public:
    SE_ImageDataHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_UnitWidthHandler : public SE_XmlElementHandler
{
public:
    SE_UnitWidthHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
   virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent); 
};
class SE_UnitHeightHandler : public SE_XmlElementHandler
{
public:
    SE_UnitHeightHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);  
};
class SE_CoordHandler : public SE_XmlElementHandler
{
public:
    SE_CoordHandler(SE_ElementManager* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
*/

class SE_ElementManager
{
public:
    SE_ElementManager();
    ~SE_ElementManager();
    void load(const char* filePath);
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
    SE_Element* findByName(const char* name);
private:
    SE_ElementManager(const SE_ElementManager&);
    SE_ElementManager& operator=(const SE_ElementManager&);
private:
    SE_Element* mRoot;
    SE_ElementMapManager mElementMapManager;
	SE_ElementMap* mCurrElementMap;
};
#endif
