#include "SE_ElementContent.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_2DElement.h"
#include "SE_ElementSchema.h"
void SE_ElementContent::clone(SE_ElementContent* src, SE_ElementContent* dst)
{
	dst->mContentURI = src->mContentURI;
	dst->mID = src->mID;
    dst->mState = src->mState;
}
SE_ElementContent* SE_ElementContent::clone()
{
	return NULL;
}
///////////////////////////////////////
SE_ImageContent::SE_ImageContent(const SE_StringID& imageURI)
{
    setURI(imageURI);
}
SE_ElementContent* SE_ImageContent::clone()
{
	SE_ImageContent* e = new SE_ImageContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_ImageContent::createElement(float mpx, float mpy)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance() ->getResourceManager();
	SE_StringID uri = getURI();
	SE_URI rui(uri.getStr());
	SE_StringID strURL = rui.getURL();
	SE_Util::SplitStringList strList = SE_Util::splitString(strURL.getStr(), "/");
	SE_XMLTABLE_TYPE t = resourceManager->getXmlType(strList[0].c_str());
	SE_Element* rete = NULL;
	if(t == SE_IMAGE_TABLE)
	{
	    SE_ImageElement* imageElement = new SE_ImageElement(getURI());
		imageElement->setMountPoint(mpx, mpy);
		rete = imageElement;
	}
	else if(t == SE_ELEMENT_TABLE)
	{
        SE_TextureElement* textureElement = new SE_TextureElement(uri);
		textureElement->setMountPoint(mpx, mpy);
		rete = textureElement;
	}
	else if(t == SE_COLOREFFECT_TABLE)
	{
		SE_ColorEffectControllerElement* colorEffectControllerElement = new SE_ColorEffectControllerElement(getURI());
		colorEffectControllerElement->setMountPoint(mpx, mpy);
		rete = colorEffectControllerElement;
	}
    rete->setName(getID().getStr());
    rete->setState(getState());
	return rete;
}
SE_ActionContent::SE_ActionContent(const SE_StringID& actionURI)
{
	setURI(actionURI);
}
SE_ElementContent* SE_ActionContent::clone()
{
	SE_ActionContent* e = new SE_ActionContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_ActionContent::createElement(float mpx, float mpy)
{
	SE_ActionElement* e = new SE_ActionElement(getURI());
	e->setMountPoint(mpx, mpy);
    e->setState(getState());
	return e;
}
SE_StateTableContent::SE_StateTableContent(const SE_StringID& stateTableURI)
{
	setURI(stateTableURI);
}
SE_ElementContent* SE_StateTableContent::clone()
{
	SE_StateTableContent* e = new SE_StateTableContent(this->getURI());
	SE_ElementContent::clone(this, e);
	return e;
}
SE_Element* SE_StateTableContent::createElement(float mpx, float mpy)
{
	SE_StateTableElement* e = new SE_StateTableElement(getURI());
	e->setMountPoint(mpx, mpy);
    e->setState(getState());
	return e;
}

