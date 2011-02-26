#include "SE_ElementSchema.h"
#include "SE_Application.h"
#include "SE_Element.h"
#include "SE_ElementManager.h"
#include "SE_ElementContent.h"
#include "SE_Utils.h"
#include "SE_ElementType.h"
#include "SE_TextView.h"
#include "SE_Button.h"
#include <algorithm>
SE_ElementSchema::SE_ElementSchema()
{
	x = y = w = h = 0;
	pivotx = pivoty = 0;
	seq = -1;
    type = SE_2D_UI_NODE;
	patchType = SE_NO_PATCH;
}
SE_ElementSchema::~SE_ElementSchema()
{
	clear();
}
void SE_ElementSchema::clear()
{
    _ElementContentList::iterator it;
    for(it = contents.begin() ; it != contents.end() ; it++)
    {
        SE_ElementContent* ec = *it;
        delete ec;
    } 
    _ElementSchemaList::iterator itSchema;
    for(itSchema = children.begin() ; itSchema != children.end() ; itSchema++)
    {
        SE_ElementSchema* es = *itSchema;
		es->clear();
		delete es;
    }
}
SE_2DNodeElement* SE_ElementSchema::createElement(int type)
{
    switch(type)
    {
    case SE_2D_UI_NODE:
        return new SE_2DNodeElement;
    case SE_UI_BUTTON:
        return new SE_Button;
    case SE_UI_TEXTVIEW:
        return new SE_TextView;
    default:
        break;
    }
    return NULL;
}
SE_Element* SE_ElementSchema::createElement(SE_ElementManager* elementManager, SE_Element* parent)
{
    SE_2DNodeElement* e = createElement(type); 
    e->setName(name.getStr());
    e->setFullPathName(fullPathName.getStr());
    e->setMountPointRef(mountPointRef);
    e->setMountPoint(mountPointSet);
    e->setPivotX(pivotx);
    e->setPivotY(pivoty);
	e->setWidth(w);
	e->setHeight(h);
    e->setSeqNum(seq);
	e->setLocalLayer(layer);
	e->setRectPatchType(patchType);
    SE_Element* root = NULL;
    if(parent == NULL)
        root = e;
	else
        elementManager->add(parent, e);

    SE_ASSERT(children.empty() || contents.empty());
    _ElementContentList::iterator it;
    for(it = contents.begin() ; it != contents.end() ; it++)
    {
        SE_ElementContent* ec = *it;
        SE_Element* childElement = ec->createElement(0, 0);
        elementManager->add(e, childElement);
    } 
    _ElementSchemaList::iterator itSchema;
    for(itSchema = children.begin() ; itSchema != children.end() ; itSchema++)
    {
        SE_ElementSchema* es = *itSchema;
        es->createElement(elementManager, e); 
    }
    return root;
}
SE_Element* SE_ElementSchema::createElement()
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    SE_Element* root = createElement(elementManager, NULL);
	return root;
} 
void SE_ElementSchema::addChild(SE_ElementSchema* ec)
{
    children.push_back(ec);
}
void SE_ElementSchema::setParent(SE_ElementSchema* p)
{
    parent = p;
}
void SE_ElementSchema::addContent(SE_ElementContent* ec)
{
    contents.push_back(ec);
}
void SE_ElementSchema::travel(SE_ElementSchemaVisitor* v)
{
	v->visit(this);
	_ElementSchemaList::iterator it;
	for(it = children.begin() ; it != children.end() ; it++)
	{
		SE_ElementSchema* ec = *it;
		ec->travel(v);
	}
}
SE_ElementContent* SE_ElementSchema::getContent(int index)
{
	_ElementContentList::const_iterator it = listElementRef(contents, index);
	if(it != contents.end())
	{
		return *it;
	}
	else
		return NULL;
}
