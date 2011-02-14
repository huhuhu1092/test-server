#ifndef SE_ELEMENTSCHEMA_H
#define SE_ELEMENTSCHEMA_H
#include <list>
#include "SE_ID.h"
#include "SE_TableManager.h"
#include "SE_Layer.h"
#include "SE_MountPoint.h"
class SE_ElementSchemaVisitor
{
public:
	virtual void visit(SE_ElementSchema* e) = 0;
};
class SE_Element;
class SE_ElementManager;
class SE_ElementContent;
class SE_ElementSchema
{
public:
    SE_Element* createElement();
    void addChild(SE_ElementSchema* ec);
    void setParent(SE_ElementSchema* p);
    void addContent(SE_ElementContent* ec);
	int getContentNum()
	{
		return contents.size();
	}
	SE_ElementContent* getContent(int index);
	void travel(SE_ElementSchemaVisitor* v);
private:
    SE_Element* createElement(SE_ElementManager* elementManager, SE_Element* parent);
public:
    SE_StringID name;
    SE_StringID fullPathName;
    SE_MountPointSet mountPointSet;
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
typedef SE_Table<SE_StringID, SE_ElementSchema*> SE_ElementSchemaMap;
typedef SE_Table<SE_StringID, SE_ElementSchemaMap*> SE_ElementSchemaTable;
#endif
