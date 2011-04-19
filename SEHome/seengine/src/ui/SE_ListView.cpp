#include "SE_ListView.h"
#include "SE_Math.h"
SE_ListView::SE_ListView(STYLE s, const char* listItemSchema)
{
	mStyle = s;
	mListItemSchema = listItemSchema;
	mCurrPos = 0;
	mItemNum = 0;
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
	clearChildren();
	mListItemList.clear();
	SE_2DNodeElement::spawn();
	SE_ListItem* listItem = createListItem(mListItemSchema.c_str());
	int ret = mDataSource->fillItem(listItem, mCurrPos);
	if(ret != SE_ListViewDataSource::OK)
	{
		delete listItem;
		return;
	}
	listItem->spawn();
	listItem->layout();
	float width = listItem->getWidth();
	float height = listItem->getHeight();
	int num = 0;
	if(mStyle == HORIZONTAL)
	{
		num = calculateItemNum(getWidth(), width);
	}
	else if(mStyle == VERTICAL)
	{
		num = claculateItemNum(getHeight(), height);
	}
	else
	{
		LOGI("## error: list view's style can not support ##\n");
		return;
	}
	SE_ASSERT(num != 0);
	mItemNum = num;
	SE_ElementManager* elementManager = SE_GET_ELEMENTMANAGER();
	elementManager->add(this->getID(), listItem);
	calculateItemDimension(listItem);
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
		    calculateItemDimension(listItem);
			calculateItemMountPoint(listItem, startx, starty, width, height);
		    mCurrPos++;
		}
		else
		{
			delete listItem;
		}
	}
}
void SE_ListView::calculateItemMountPoint(SE_ListItem* listItem, float& startx, float& starty , float itemWidth, float itemHeight)
{
	listItem->setMountPoint(startx, starty);
	if(mStyle == HORIZONTAL)
	{
		startx += itemWidth;
	}
	else if(mStyle == VERTICAL)
	{
		starty += itemHeight;
	}
}
void SE_ListView::calculateItemDimension(SE_ListItem* listItem)
{
	if(mStyle == HORIZONTAL)
	{
		listItem->setHeight(getHeight());
	}
	else if(mStyle == VERTICAL)
	{
		listItem->setWidth(getWidth());
	}
	else
	{
		SE_ASSERT(0);
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
//add new item when scroll to left
//if scroll to left, maybe add some item to the rightest item
std::list<SE_ListItem*> SE_ListView::addNewItemList(const SE_Vector2f& v)
{
	float right = getRight();
    if(v.x >= right)
		return std::list<SE_ListItem*>();
	float gap = right - v.x;
	
}
void SE_ListView::addNewItem(std::list<_ListItemData>& itemList)
{
	if(mStyle == HORIZONTAL)
	{
		if(mScrollType == SCROLL_TO_LEFT)
		{
			SE_Vector2f v = getRightestItemPos();
			v = v + SE_Vector2f(mItemWidth, 0);
			std::list<SE_ListItem*> newList = addNewItemList(v);
		}
		else if(mScrollType == SCROLL_TO_RIGHT)
		{
			
		}
	}
	else if(mStyle == VERTICAL)
	{
		
	}
	else
	{
		SE_ASSERT(0);
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
		else if(mStyle == VERTICAL)
		{
			itCoord->v.y += deltay;
		}
		else
		{
			SE_ASSERT(0);
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
