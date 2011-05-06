#include "SE_ListItem.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_ImageElement.h"
SE_ListItem* SE_ListItem::createFromSchema(const char* schema)
{
	SE_ResourceManager* resourceManager = SE_GET_RESOURCEMANAGER();
	SE_ElementSchema* elementSchema = resourceManager->getElementSchema();
	if(elementSchema)
	{
		return elementSchema->createElement();
	}
	else
		return NULL;
}
#define SET_IMAGE(name, iu) \
    SE_ImageElement* imageElement = (SE_ImageElement*)findByName(name);	\
	if(imageElement) \
	{ \
		imageElement->setBaseColor(iu); \
	}
void SE_ListItem::setImage1(const SE_ImageUnit& iu)
{
    SET_IMAGE("image1", iu);
}
void SE_ListItem::setImage2(const SE_ImageUnit& iu)
{
    SET_IMAGE("image2", iu);
}
void SE_ListItem::setImage3(const SE_ImageUnit& iu)
{
	SET_IMAGE("image3", iu);
}
void SE_ListItem::setImage4(const SE_ImageUnit& iu)
{
	SET_IMAGE("image4", iu);
}
void SE_ListItem::setText1(const SE_StringID& str)
{

}
void SE_ListItem::setText2(const SE_StringID& str)
{}
void SE_ListItem::setText3(const SE_StringID& str)
{}
void SE_ListItem::setText4(const SE_StringID& str)
{}