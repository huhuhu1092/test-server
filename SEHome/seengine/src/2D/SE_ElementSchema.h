#ifndef SE_ELEMENTSCHEMA_H
#define SE_ELEMENTSCHEMA_H
#include <list>
#include "SE_ID.h"
class SE_Element;
class SE_ElementManager;
class SE_ElementSchema
{
public:
    SE_Element* createElement();
    void addChild(SE_ElementSchema* ec);
    void setParent(SE_ElementSchema* p);
    void addContent(SE_ElementContent* ec);
private:
    SE_Element* createElement(SE_ElementManager* spatialManager, SE_Element* parent);
public:
    SE_StringID name;
    SE_StringID fullPathName;
    SE_MountPiontSet mountPointSet;
    float pivotx, pivoty;
    float x, y, w, h;
    SE_Layer layer;
    int seq;
    SE_MountPointID mountpointref;
private:
    SE_ElementSchema* parent;
    typedef std::list<SE_ElementSchema*> _ElementSchemaList;
    _ElementSchemaList children;
    typedef std::list<SE_ElementContent*> _ElementContentList;
    _ElementContentList contents;
};
#endif
