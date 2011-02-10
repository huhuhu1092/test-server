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
private:
    void createElement(SE_ElementManager* spatialManager, SE_Element* parent);
public:
    SE_StringID name;
    SE_StringID fullPathName;
    SE_MountPiontSet mountPointSet;
    typedef std::list<SE_ElementSchema*> _ElementSchemaList;
    _ElementSchemaList children;
    float pivotx, pivoty;
    SE_MountPointID mountpointref;
    typedef std::list<SE_ElementContent*> _ElementContentList;
    _ElementContentList contents;
    int seq;
    SE_Element* root;
};
#endif
