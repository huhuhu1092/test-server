#include "SE_StateTableElement.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_Log.h"
#include "SE_Spatial.h"
#include "SE_SpatialManager.h"
#include "SE_ElementManager.h"
#include "SE_StateTable.h"
#include "SE_2DElement.h"
SE_StateTableElement::SE_StateTableElement(const SE_StringID& uri)
{
	setURI(uri);
	SE_StringID url = getURL();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mStateTable = resourceManager->getStateMachine(url.getStr());
}
SE_Element* SE_StateTableElement::clone()
{
	SE_StateTableElement* element = new SE_StateTableElement(getURI());
	SE_2DNodeElement::clone(this, element);
	return element;
}
void SE_StateTableElement::update(SE_ParamValueList& paramValueList)
{
	SE_StringID url = getURL();
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mStateTable = resourceManager->getStateMachine(url.getStr());   
    clearChildren();
    spawn();
    layout();
    update(0);
    SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* currSpatial = spatialManager->get(getID());
    if(!currSpatial)
    {
        LOGI("state table element spatial has been removed\n");
        return;
    }
    clearChildren();
	std::vector<SE_Element*> children = getChildren();
	std::vector<SE_Element*>::iterator it;
    for(it = children.begin(); it != children.end() ; it++)
    {
        SE_Element* e = *it;
        SE_Spatial* s = e->createSpatial();
        spatialManager->add(currSpatial->getID(), s, true); 
    }
}
void SE_StateTableElement::update(const SE_AddressID& address, const SE_Value& value)
{
    SE_2DNodeElement::update(address, value);
}
void SE_StateTableElement::update(const SE_TimeKey& key)
{}
void SE_StateTableElement::spawn()
{
    calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
	SE_State* currState = mStateTable->getCurrentState();
	SE_ASSERT(currState != NULL);
	if(!currState)
		return;
	SE_StringID strURI = currState->getDefaultValue();
	SE_2DNodeElement* e = (SE_2DNodeElement*)SE_GetElement(strURI);
    e->setMountPoint(0, 0);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	elementManager->add(getID(), e, true);
	//this->addChild(e);
	e->spawn();
}
void SE_StateTableElement::layout()
{
	SE_Element::layout();
}
SE_Spatial* SE_StateTableElement::createSpatial()
{
	return SE_Element::createSpatial();
}
