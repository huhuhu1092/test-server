#include "SE_ListItem.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
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
