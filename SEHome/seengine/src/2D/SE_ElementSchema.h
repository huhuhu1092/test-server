#ifndef SE_ELEMENTSCHEMA_H
#define SE_ELEMENTSCHEMA_H
#include <list>
#include <string>
#include "SE_ID.h"
#include "SE_TableManager.h"
#include "SE_Layer.h"
#include "SE_MountPoint.h"
#include "SE_Vector.h"
class SE_ElementSchema;
class SE_2DNodeElement;
class SE_ElementSchemaVisitor
{
public:
    virtual ~SE_ElementSchemaVisitor() {}
	virtual void visit(SE_ElementSchema* e) = 0;
};
class SE_ElementProperty
{
public:
    virtual ~SE_ElementProperty() {}
};
class SE_TextProperty : public SE_ElementProperty
{
public:
    std::string style;
    SE_Vector3i color;
    int size;
    std::string align;
    std::string orientation;
    std::string state;
};
class SE_Element;
class SE_ElementManager;
class SE_ElementContent;
class SE_ElementSchema
{
public:
	SE_ElementSchema();
	~SE_ElementSchema();
    SE_Element* createElement();
    void addChild(SE_ElementSchema* ec);
    void setParent(SE_ElementSchema* p);
    void addContent(SE_ElementContent* ec);
    void addProperty(SE_ElementProperty* p);
	size_t getContentNum()
	{
		return contents.size();
	}
	SE_ElementContent* getContent(int index);
	void travel(SE_ElementSchemaVisitor* v);
    std::string getSeqNum() const
	{
		return seq;
	}
	static int getElementType(const SE_StringID& type);
	static int getElementState(const SE_StringID& state);
private:
    SE_Element* createElement(SE_ElementManager* elementManager, SE_Element* parent);
	void clear();
    SE_2DNodeElement* createElement(int type);
public:
    SE_StringID name;
    SE_StringID fullPathName;
    SE_MountPointSet mountPointSet;
    float pivotx, pivoty;
    float x, y, w, h;
    SE_Layer layer;
    std::string seq;
    SE_MountPointID mountPointRef;
    int type;// element type: BUTTON, TEXT, 2D_UI_NODE, etc;
    SE_StringID text;
    int state;
	SE_RECTPATCH_TYPE patchType;
    bool canpointed;
private:
    SE_ElementSchema* parent;
    typedef std::list<SE_ElementSchema*> _ElementSchemaList;
    _ElementSchemaList children;
    typedef std::list<SE_ElementContent*> _ElementContentList;
    _ElementContentList contents;
    typedef std::list<SE_ElementProperty*> _ElementPropertyList;
    _ElementPropertyList properties;
    
};
typedef SE_Table<SE_StringID, SE_ElementSchema*> SE_ElementSchemaMap;
typedef SE_Table<SE_StringID, SE_ElementSchemaMap*> SE_ElementSchemaTable;
#endif
