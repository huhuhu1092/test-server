#ifndef SE_LISTVIEW_H
#define SE_LISTVIEW_H
#include "SE_2DNodeElement.h"
class SE_ListItem : public SE_2DNodeElement
{
public:
    static SE_ListItem* createFromSchema(const char* schema);
};
class SE_ListViewDataSource
{
public:
    enum {OK, END_OF_DATASOURCE, WAIT_FOR_UPDATE};	
	virtual ~SE_ListDataSource() {}
	virtual int fillItem(SE_ListItem* listItem, unsigned int pos) = 0;
};
class SE_ListView : public SE_2DNodeElement
{
public:
	typedef unsigned int INDEX;
	enum STYLE {HORIZONTAL, VERTICAL};
	SE_ListView(STYLE s, const char* listItemSchema);
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
	_ItemList mListItemList;
	float mItemWidth;
	float mItemHeight;
};
#endif
