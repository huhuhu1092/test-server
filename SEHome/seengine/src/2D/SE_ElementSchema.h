#ifndef SE_ELEMENTSCHEMA_H
#define SE_ELEMENTSCHEMA_H
class SE_ElementSchema
{
public:
    SE_StringID id;
    SE_MountPiontSet mountPointSet;
    typedef std::list<SE_ElementSchema*> _ElementSchemaList;
    _ElementSchemaList children;
};
#endif
