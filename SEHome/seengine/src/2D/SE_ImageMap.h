#ifndef SE_IMAGEMAP_H
#define SE_IMAGEMAP_H
#include "SE_ID.h"
#include "SE_TableManager.h"
#include "SE_Common.h"
struct SE_ImageRect
{
	enum {NO_MIRROR, MIRROR_X, MIRROR_Y, MIRROR_XY};
    enum COLOR_CHANNEL {ALL, R, G, B, A, CHANNEL_NUM};
    int x;
    int y;
    int width;
    int height;
    int mirrorType;
	int pivotx;
	int pivoty;
	int index;
	COLOR_CHANNEL c;
	SE_ImageRect()
    {
        x = y = 0;
        width = height = INVALID_GEOMINFO;
		pivotx = pivoty = INVALID_GEOMINFO;
		mirrorType = NO_MIRROR;
		index = 0;
		c = ALL;
    }
};
struct SE_ImageUnit
{  
    SE_ImageRect imageRect;
	SE_ImageDataID imageDataID;
	SE_StringID imageURL; // when you get a SE_ImageUnit From SE_ResourceManager, it will 
	                      // separate your input url to imageURL and ext.
	SE_StringID ext;
	SE_ImageUnit()
	{
	}
	bool isValid() const
	{
		return imageDataID.isValid() && imageRect.width > 0 && imageRect.height > 0;
	}

};

class SE_ImageItemProperty
{
public:
	SE_ImageItemProperty()
	{
		mPivotX = INVALID_GEOMINFO;
		mPivotY = INVALID_GEOMINFO;
		mIndex = 0;
	}
    void setImageDataID(const SE_ImageDataID& id)
    {
        mImageDataID = id;
    }
    SE_ImageDataID getImageDataID() const
    {
        return mImageDataID;
    }
	void setIndex(int index)
	{
		mIndex = index;
	}
	int getIndex() const
	{
		return mIndex;
	}
	int getPivotX() const
	{
		return mPivotX;
	}
	void setPivotX(int pivotx)
	{
		mPivotX = pivotx;
	}
	int getPivotY() const
	{
		return mPivotY;
	}
	void setPivotY(int pivoty)
	{
		mPivotY = pivoty;
	}
private:
    SE_ImageDataID mImageDataID;
	int mIndex;
	int mPivotX;
	int mPivotY;
    //SE_ObjectManager<SE_StringID, SE_ImageRect> mImageRectSet;
};
typedef SE_Table<SE_StringID, SE_ImageRect, SE_ImageItemProperty> SE_ImageItem;
typedef SE_Table<SE_StringID, SE_ImageItem*> SE_ImageMap;
typedef SE_Table<SE_StringID, SE_ImageMap*> SE_ImageMapSet;
typedef SE_Table<SE_StringID, SE_ImageMapSet*> SE_ImageTable;
struct SE_ImageTableVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageMapSet*>
{
};
struct SE_ImageMapSetVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageMap*>
{
	SE_StringID imageMapSetID;
};
struct SE_ImageMapVisitor : public SE_ObjectManagerVisitor<SE_StringID , SE_ImageItem*>
{
	SE_StringID imageMapSetID;
	SE_StringID imageMapID;
};
struct SE_ImageItemVisitor : public SE_ObjectManagerVisitor<SE_StringID, SE_ImageRect>
{
	SE_StringID imageMapSetID;
	SE_StringID imageMapID;
	SE_StringID imageItemID;
};
#endif
