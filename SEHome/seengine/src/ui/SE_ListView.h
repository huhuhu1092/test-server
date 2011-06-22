#ifndef SE_LISTVIEW_H
#define SE_LISTVIEW_H
#include "SE_2DNodeElement.h"
#include "SE_ListItem.h"
#include "SE_ListViewDataSource.h"

class SE_ListView : public SE_2DNodeElement
{
public:
	typedef unsigned int INDEX;
	enum STYLE {HORIZONTAL, VERTICAL};
	SE_ListView(STYLE s, const char* listItemSchema, SE_ListViewDataSource* dataSource = NULL);
	~SE_ListView();
    void setStyle(STYLE s)
	{
		mStyle = s;
	}
	STYLE getStyle() const
	{
		return mStyle;
	}
	void setListDataSource(SE_ListViewDataSource* dataSource)
	{
		mDataSource = dataSource;
	}
    void setListItemSchema(const char* schema)
	{
		mListItemSchema = schema
	}
    void spawn();
	void layout();
	SE_Spatial* createSpatial();
	//deltax < 0 mean scroll to left else scroll to right
	//deltay < 0 mean scroll to top else scroll to bottom
	void update(float deltax, float deltay);
private:
    enum {SCROLL_TO_TOP, SCROLL_TO_BOTTOM, SCROLL_TO_LEFT,
	      SCROLL_TO_RIGHT};
	enum LISTITEM_OP {_CURRPOS_ADD, _CURRPOS_SUBTRACT};
    //this struct is used by update
	//it record the coordinate of list item
	//when list item update its x, y coordinate
	//some list items will be outside of list view
	//we will clip these list items
	struct _ListItemData
	{
		SE_Vector2f v;
		SE_ListItem* item;
		bool cliped;
		_ListItemData()
		{
			item = NULL;
			cliped = false;
		}
	};
private:
    SE_ListItem* createListItem(const char* listItemSchema);	
private:
	STYLE mStyle;
	SE_ListViewDataSource* mDataSource;
	std::string mListItemSchema;
	INDEX mCurrPos;
	int mItemNum = 0;
	typedef std::list<SE_ListItem*> _ItemList;
	// the list of item which is in list view
	// the list item outof list view will not
	// be in mListItemList
	_ItemList mListItemList;
	float mItemWidth;
	float mItemHeight;
};
#endif
