#include "SE_Button.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_Spatial.h"
#include "SE_2DElement.h"
SE_Button::SE_Button()
{
	mElement = NULL;
}
SE_Button::~SE_Button()
{}
void SE_Button::spawn()
{
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
    if(mElement)
	{
		elementManager->remove(mElement->getID());
		elementManager->release(mElement, SE_RELEASE_NO_DELAY);
		mElement = NULL;
	}
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
	return SE_Widget::createSpatial();
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
