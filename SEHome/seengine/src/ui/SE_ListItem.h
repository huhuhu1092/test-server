#ifndef SE_LISTITEM_H
#define SE_LISTITEM_H
#include "SE_2DNodeElement.h"
class SE_ListItem : public SE_2DNodeElement
{
public:
    static SE_ListItem* createFromSchema(const char* schema);	
};
#endif
