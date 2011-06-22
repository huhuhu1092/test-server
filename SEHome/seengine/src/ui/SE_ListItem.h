#ifndef SE_LISTITEM_H
#define SE_LISTITEM_H
#include "SE_2DNodeElement.h"
#include "SE_ID.h"
class SE_ImageUnit;
class SE_ListItem : public SE_2DNodeElement
{
public:
    static SE_ListItem* createFromSchema(const char* schema);	
	void setImage1(const SE_ImageUnit& iu);
	void setImage2(const SE_ImageUnit& iu);
	void setImage3(const SE_ImageUnit& iu);
	void setImage4(const SE_ImageUnit& iu);
	void setText1(const SE_StringID& str);
	void setText2(const SE_StringID& str);
	void setText3(const SE_StringID& str);
	void setText4(const SE_StringID& str);
};
#endif
