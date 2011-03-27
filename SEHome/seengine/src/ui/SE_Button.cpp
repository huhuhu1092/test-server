#include "SE_Button.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_Spatial.h"
#include "SE_2DElement.h"
#include "SE_Utils.h"
SE_Button::SE_Button()
{
	mElement = NULL;
}
SE_Button::~SE_Button()
{}
void SE_Button::spawn()
{
	std::vector<SE_Element*> children = getChildren();
	if(children.size() <= 0)
		return;
	SE_2DNodeElement* element = NULL;
	for(int i = 0 ; i < children.size() ; i++)
	{
		SE_2DNodeElement* e = (SE_2DNodeElement*)children[i];
		e->setCanPointed(false);
	    e->setNeedUpdateStateFromParent(false);
		std::string str;
		std::string strIndex = SE_Util::intToString(i);
		SE_StringID stateName = e->getStateName(e->getState());
		str = std::string("button_") + stateName.getStr() + "_" + strIndex;
		e->setName(str.c_str());
	}
    for(int i = 0 ; i < children.size() ; i++)
	{
		SE_2DNodeElement* e = (SE_2DNodeElement*)children[i];
		if(e->getState() == mState)
		{
			element = e;
			break;
		}
	}
	if(!element)
        return;

	element->spawn();
	element->setWidth(mWidth);
	element->setHeight(mHeight);
	mElement = element;
	//mElement->setRectPatchType(SE_PATCH_R3_C3);
	calculateRect(mPivotX, mPivotY, 0, 0);
	/*
    if(mElement)
		mElement->dismiss();
    SE_StringID uri = getImageURI();
	mElement = SE_GetElement(uri);
	SE_Element* parentElement = this->getParent();
	if(parentElement)
	{
		mElementID = elementManager->add(parentElement->getID(), mElement, true);
	}
	else
		mElementID = elementManager->add(SE_ElementID::INVALID, mElement, true);

	mElement->setMountPoint(0, 0);
	mElement->spawn();
	*/
}
void SE_Button::update(const SE_TimeKey& timeKey)
{}
void SE_Button::update(SE_ParamValueList& paramValueList)
{}
void SE_Button::update(const SE_AddressID& address, const SE_Value& value)
{}
void SE_Button::layout()
{
	SE_Widget::layout();
}
SE_Spatial* SE_Button::createSpatial()
{
	//return SE_Widget::createSpatial();
	SE_Spatial* parent = createNode();
	parent->setName(getName().getStr());
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
	if(mElement)
	{
		SE_Spatial* s = mElement->createSpatial();
		spatialManager->add(parent, s);
	}
	return parent;
}
void SE_Button::read(SE_BufferInput& inputBuffer)
{}
void SE_Button::write(SE_BufferOutput& outputBuffer)
{}
SE_Element* SE_Button::clone()
{
	return NULL;
}
void SE_Button::clone(SE_Element* src, SE_Element* dst)
{}
SE_StringID SE_Button::getImageURI()
{
	SE_URI uri = getURI(getState());
	return uri.getURI();
}
