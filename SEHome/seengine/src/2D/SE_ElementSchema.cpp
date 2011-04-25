#include "SE_ElementSchema.h"
#include "SE_Application.h"
#include "SE_Element.h"
#include "SE_ElementManager.h"
#include "SE_ResourceManager.h"
#include "SE_ElementContent.h"
#include "SE_Utils.h"
#include "SE_ElementType.h"
#include "SE_TextView.h"
#include "SE_Button.h"
#include "SE_CharStyle.h"
#include <algorithm>
SE_ElementSchema::SE_ElementSchema()
{
	x = y = w = h = 0;
	pivotx = pivoty = 0;
	seq = -1;
    type = SE_2D_UI_NODE;
	patchType = SE_NO_PATCH;
    canpointed = true;
    state = SE_Element::NORMAL;
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
    _ElementPropertyList::iterator itProperty;
    for(itProperty = properties.begin() ; itProperty != properties.end();  itProperty++)
    {
        delete *itProperty  ;  
    }
}
static SE_TextView::ALIGN getTextAlign(std::string str)
{
    if(str == "left")
        return SE_TextView::LEFT;
    else if(str == "right")
        return SE_TextView::RIGHT;
    else if(str == "mid")
        return SE_TextView::MID;
    else if(str == "top")
        return SE_TextView::TOP;
    else if(str == "BOTTOM")
        return SE_TextView::BOTTOM;
    else
        return SE_TextView::LEFT;
}
static SE_TextView::ORIENTATION getTextOrientation(std::string str)
{
    if(str == "vertical")
        return SE_TextView::VERTICAL;
    else if(str == "horizontal")
        return SE_TextView::HORIZONTAL;
    else
        return SE_TextView::HORIZONTAL;
}
static int getTextState(std::string state)
{
	return SE_Element::getStateFromName(state.c_str());
}
SE_2DNodeElement* SE_ElementSchema::createElement(int type)
{
    switch(type)
    {
    case SE_2D_UI_NODE:
        return new SE_2DNodeElement;
    case SE_UI_BUTTON:
        return new SE_Button;
	case SE_UI_LISTITEM:
		return new SE_ListItem;
	case SE_UI_LISTVIEW:
		{
			return NULL;
		}
    case SE_UI_TEXTVIEW:
        {
			SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
            SE_TextView* tv = new SE_TextView;
			SE_StringID str = resourceManager->getString(text.getStr());
			tv->setText(str);
            _ElementPropertyList::iterator it;
            for(it = properties.begin() ; it != properties.end() ; it++)
            {
                SE_TextProperty* textp = (SE_TextProperty*)*it;
                int state = getTextState(textp->state);
                tv->setAlign(state, getTextAlign(textp->align));
                tv->setOrientation(state,getTextOrientation(textp->orientation));
                tv->setFontColor(state,textp->color);
                tv->setFontSize(state,textp->size);
				tv->setCharStyle(state, SE_CharStyle(textp->style));
            }
			return tv;
        }
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
    e->setCanPointed(canpointed);
    e->setState(state, false);
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
		if(es)
		{
            es->createElement(elementManager, e); 
		}
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
void SE_ElementSchema::addProperty(SE_ElementProperty* p)
{
    properties.push_back(p);
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
int SE_ElementSchema::getElementType(const SE_StringID& type)
{
    if(type == "button")
    {
        return SE_UI_BUTTON;
    }
    else if(type == "textview")
    {
        return SE_UI_TEXTVIEW;
    }
	else if(type == "listitem")
	{
		return SE_UI_LISTITEM;
	}
	else if(type == "listview")
	{
		return SE_UI_LISTVIEW;
	}
    else
        return SE_2D_UI_NODE;	
}
int SE_ElementSchema::getELementState(const SE_StringID& state)
{
    if(state == "normal")
        return SE_Element::NORMAL;
    else if(state == "highlighted")
        return SE_Element::HIGHLIGHTED;
    else if(state == "selected")
        return SE_Element::SELECTED;
    else if(state == "inactive")
        return SE_Element::INACTIVE;
    else if(state == "")
        return SE_Element::NORMAL;
	else
		return SE_Element::INVALID;
	
}
