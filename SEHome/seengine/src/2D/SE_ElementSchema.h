#ifndef SE_ELEMENTSCHEMA_H
#define SE_ELEMENTSCHEMA_H
class SE_ElementSchema
{
public:
    SE_Element* createElement();
public:
    SE_StringID id;
    SE_MountPiontSet mountPointSet;
    typedef std::list<SE_ElementSchema*> _ElementSchemaList;
    _ElementSchemaList children;
    float pivotx, pivoty;
    SE_MountPointID mountpointref;
    typedef std::list<std::string> _ElementContentList;
    _ElementContentList contents;
    int seq;
};
#endif
