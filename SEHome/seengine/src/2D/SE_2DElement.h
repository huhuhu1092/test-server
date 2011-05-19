#ifndef SE_2DELEMENT_H
#define SE_2DELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
#include "SE_TableManager.h"
/*
    SE_Element is a tree structure. SE_Element is the node , it can has child.
	An element which has content can not has child, it responsibility is just to create
	a real spatial for display.
*/

extern SE_2DNodeElement* SE_GetElement(const SE_StringID& uri);
typedef SE_Table<SE_StringID, SE_Element*> SE_ElementMap;
typedef SE_Table<SE_StringID, SE_ElementMap*> SE_ElementTable;

#endif
