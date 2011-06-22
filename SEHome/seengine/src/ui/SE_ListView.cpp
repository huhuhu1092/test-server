#include "SE_ListView.h"
#include "SE_Math.h"
SE_ListView::SE_ListView(STYLE s, const char* listItemSchema, SE_ListViewDataSource* dataSource)
{
	mStyle = s;
	mDataSource = dataSource;
	mListItemSchema = listItemSchema;
	mCurrPos = 0;
	mItemNum = 0;
	mItemWidth = mItemHeight = 0;
}
SE_ListItem* SE_ListView::createListItem(const char* listItemSchema)
{
	SE_ListItem* listItem = SE_ListItem::createFromSchema(listItemSchema);
	listItem->setPivotX(0);
	listItem->setPivotY(0);
	listItem->setMountPoint(0, 0);
	return listItem;
}

int SE_ListView::calculateItemNum(float listViewX, float listItemX)
{
	float x = listViewX / listItemX;
	return (int)SE_Ceil(x);
}
void SE_ListView::spawn()
{
	if(!mDataSource)
		return;
	//every time spawn,
	//it firstly clear its children because it will
	//create new in child in spawn
	clearChildren();
	mListItemList.clear();
	SE_2DNodeElement::spawn();
	//create a list item from the list item schema
	SE_ListItem* listItem = createListItem(mListItemSchema.c_str());
	//data source will fill item according to item schema
	//data source will match list item schema
	int ret = mDataSource->fillItem(listItem, mCurrPos);
	if(ret != SE_ListViewDataSource::OK)
	{
		delete listItem;
		return;
	}
	SE_ElementManager* elementManager = SE_GET_ELEMENTMANAGER();
	elementManager->add(this->getID(), listItem);
	//spawn the list item
	listItem->spawn();
	listItem->update(0);
	//list item do layout,
	//this will get the list item content size
	listItem->layout();
	float width = listItem->getWidth();
	float height = listItem->getHeight();
	int num = 0;
	if(mStyle == HORIZONTAL)
	{
		num = calculateItemNum(getWidth(), width);
	}
	else
	{
		num = claculateItemNum(getHeight(), height);
	}
	SE_ASSERT(num != 0);
	//set item num which can be seen in list view
	mItemNum = num;
	setItemDimension(listItem);
	mCurrPos++;
	mListItemList.push_back(listItem);
	float startx = 0;
	float starty = 0;
	for(int i = 1 ; i < mItemNum ; i++)
 	{
	    listItem = createListItem(mListItemSchema);
		int ret= mDataSource->fillItem(listItem, mCurrPos);
		if(ret == SE_ListDataSource::OK)
		{
		    elementManager->add(this->getID(), listItem);
		    mListItemList.push_back(listItem);
		    setItemDimension(listItem);
		    setItemMountPoint(listItem, startx, starty, width, height);
		    mCurrPos++;
		}
		else
		{
			delete listItem;
		}
	}
}
void SE_ListView::setItemMountPoint(SE_ListItem* listItem, float& startx, float& starty , float itemWidth, float itemHeight, LISTITEM_OP op)
{
	listItem->setMountPoint(startx, starty);
	if(mStyle == HORIZONTAL)
	{
		if(op == _CURRPOS_ADD)
		{
		    startx += itemWidth;
		}
		else
		{
			startx -= itemWidth;
		}
	}
	else
	{
		if(op == _CURRPOS_ADD)
		{
		    starty += itemHeight;
		}
		else
		{
		    starty -= itemHeight;	
		}
	}
}
void SE_ListView::setItemDimension(SE_ListItem* listItem)
{
	if(mStyle == HORIZONTAL)
	{
		listItem->setHeight(getHeight());
		mItemWidth = listItem->getWidth();
		mItemHeight = getHeight();
	}
	else
	{
		listItem->setWidth(getWidth());
		mItemWidth = getWidth();
		mItemHeight = listItem->getHeight();
	}
}
void SE_ListView::layout()
{
	if(!mDataSource)
		return;
	SE_2DNodeElement::layout();
	
}
SE_Spatial* SE_ListView::createSpatial()
{
	if(!mDataSource)
		return NULL;
	return SE_2DNodeElement::createSpatial();
}

std::list<SE_ListView::_ListItemData> SE_ListView::clipListItem(std::list<_ListItemData>& itemList)
{
}

void SE_ListView::addNewItemScroll(int num , float startx, float starty, LISTITEM_OP op)
{
	for(int i = 0 ; i < num ; i++)
	{
		SE_ListItem* item = createListItem(mListItemSchema.c_str());
		int ret = mDataSource->fillItem(item, mCurrPos);
		if(ret != OK)
		{
			delete item;
		}
		else
		{
			if(LISTITEM_OP == _CURRPOS_ADD)
			{
		        mListItemList.push_back(item);
			}
			else
			{
				mListItemList.push_front(item);
			}
	 	    elementManager->add(getID(), item);
			setItemMountPoint(item, startx, starty, mItemWidth, mItemHeight, op);

			if(LISPITEM_OP == _CURRPOS_ADD)
			{
		        mCurrPos++;
			}
			else
			{
			    mCurrPos--;	
			}
		}
	}	
}
//add new item when scroll to left
//if scroll to left, maybe add some item to the rightest item
//this imply that the style is HORIZONTAL
void SE_ListView::addNewItemListScrollToLeft(const SE_Vector2f& v)
{
	SE_ASSERT(mStyle == HORIZONTAL);
	SE_ASSERT(mDataSource != NULL);
	float right = getWidth();
    if(v.x >= right)
		return ;
	float gap = right - v.x;
	int num = calculateItemNum(gap, mItemWidth);
	float startx = v.x;
	float starty = 0;
    addItemScroll(num, startx, starty, _CURRPOS_SUBTRACT);
}
void SE_ListView::addNewItemListScrollToRight(const SE_Vector2f& v)
{
    SE_ASSERT(mStyle == HORIZONTAL);
	SE_ASSERT(mDataSource != NULL);
	if(v.x <= 0)
		return;
	float gap = v.x;
	int num = calculateItemNum(gap, mItemWidth);
	float startx = v.x;
	float starty = 0;
	addItemScroll(num, startx, starty, _CURRPOS_ADD);
}
void SE_ListView::addNewItemListScrollToTop(const SE_Vector2f& v)
{
	SE_ASSERT(mStyle == VERTICAL);
	SE_ASSERT(mDataSource != NULL);
	if(v.y > getHeight())
		return;
	float gap = getHeight() - v.y;
	int num = calculateItemNum(gap, mItemHeight);
	float startx = 0;
	float starty = v.y;
	addItemScroll(num, startx, starty, _CURRPOS_ADD);
}
void SE_ListView::addNewItemListScrollToBottom(const SE_Vector2f& v)
{
	SE_ASSERT(mStyle == VERTICAL);
	SE_ASSERT(mDataSource != NULL);
	if(v.y <= 0)
		return;
	float gap = v.y;
	int num = calculateItemNum(gap, mItemHeight);
	float startx = 0;
	float starty = v.y;
	addItemScroll(num, startx, starty, _CURRPOS_SUBTRACT);
}
SE_Vector2f SE_ListView::getHighestItemPos() const
{
	if(mListItemList.empty())
		return SE_Vector2f();
	_ItemList::const_reverse_iterator it = mListItemList.rbegin();
	return SE_Vector2f((*it)->getLeft(), (*it)->getTop());	
}
SE_Vector2f SE_ListView::getLowestItemPos() const
{
	if(mListItemList.empty())
		return SE_Vector2f();
	_ItemList::const_iterator it = mListItem.begin();
	return SE_Vector2f((*it)->getLeft(), (*it)->getTop());	
}
SE_Vector2f SE_ListView::getRightItemPos() const
{
    return getHighestItemPos();
}
SE_Vector2f SE_ListView::getLeftItemPos() const
{
    return getLowestItemPos();
}
SE_Vector2f SE_ListView::getTopItemPos() const
{
	return getLowestItemPos();
}
SE_Vector2f SE_ListView::getBottomItemPos() const
{
	return getHighestItemPos();
}

void SE_ListView::addNewItem(std::list<_ListItemData>& itemList)
{
	if(mStyle == HORIZONTAL)
	{
		if(mScrollType == SCROLL_TO_LEFT)
		{
			SE_Vector2f v = getRightItemPos();
			v = v + SE_Vector2f(mItemWidth, 0);
			addNewItemListScrollToLeft(v);
		}
		else if(mScrollType == SCROLL_TO_RIGHT)
		{
			SE_Vector2f v = getLeftItemPos();
			addNewItemListScrollToRight(v);
		}
	}
	else
	{
	    if(mScrollType == SCROLL_TO_TOP)
		{
			SE_Vector2f v = getBottomItemPos();
			v = v + SE_Vector2f(0, mItemHeight);
			addNewItemListScrollToTop(v);
		}
		else if(mScrollType == SCROLL_TO_BOTTOM)
		{
			SE_Vector2f v = getTopItemPos();
			addNewItemListScrollToBottom(v);
		}
	}
}
void SE_ListView::update(float deltax, float deltay)
{
	_ItemList tmpItemList = mListItemList;
	std::list<_ListItemData> visibleItemList;
	_ItemList::iterator it;
	for(it = tmpItemList.begin() ; it != tmpItemList.end(); it++)
	{
		SE_ListItem* listItem = *it;
		SE_Vector2f v(listItem.getLeft(), listItem.getTop());
		_ListItemData lid;
		lid.v = v;
		lid.item = listItem;
		visibleItemList.push_back(lid);
	}
	std::list<_ListItemData>::iterator itCoord;
	for(itCoord = visibleItemList.begin() ; itCoord != visibleItemList.end() ; itCoord++)
	{
		if(mStyle == HORIZONTAL)
		{
			itCoord->v.x += deltax;
		}
		else
		{
			itCoord->v.y += deltay;
		}
	}
	clipListItem(visibleItemList);
	//after clipListItem
	//we will remove the cliped item from ItemList and from its
	//parent.
	SE_ElementManager* elementManager = SE_GET_ELEMENTMANAGER();
	for(itCoord = visibleItemList.begin() ; itCoord != visibleItemList.end() ; itCoord++)
	{
		if(itCoord->cliped)
		{
			SE_ASSERT(itCoord->item);
			SE_ListItem* item = elementManager->remove(itCoord->item->getID());
			elementManager->release(item);
			mListItemList.remove(itCoord->item);
		}
		else
		{
			itCoord->item->setMountPoint(itCoord->v.x, itCoord->v.y);
		}
	}
	//
	addNewItem(visibleItemList);
}
