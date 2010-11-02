#ifndef SE_IMAGEMAP_H
#define SE_IMAGEMAP_H
#include "SE_ID.h"
#include "SE_ObjectManager.h"
#include "SE_TableManager.h"
struct SE_ImageRect
{
    int x;
    int y;
    int pivotx;
    int pivoty;
    int width;
    int height;
    SE_ImageRect()
    {
        x = y = 0;
        pivotx = pivoty = 0;
        width = height = -1;
    }
};

class SE_ImageMap : public SE_Table<SE_StringID, SE_ImageRect*, SE_IDTrait<SE_StringID> >
{
public:
	/*
    typedef SE_ImageMapID TABLE_ITEM_ID;
    typedef SE_ImageRect* TABLE_ITEM_VALUE;
    typedef SE_IDTrait<SE_ImageMapID> TABLE_ITEM_IDTRAIT;
	*/
    enum {NO_MIRROR, MIRROR_X, MIRROR_Y, MIRROR_XY};
    SE_ImageMap()
    {
        mUnitWidth = -1;
        mUnitHeight = -1;
        mMirrorType = NO_MIRROR;
    }
    void setMirrorType(int mt)
    {
        mMirrorType = mt;
    }
    int getMirrorType()
    {
        return mMirrorType;
    }
    void setID(const SE_StringID& id)
    {
        mID = id;
    }
    SE_StringID getID()
    {
        return mID;
    }
    SE_ImageDataID getImageDataID()
    {
        return mImageDataID;
    }
    void setImageDataID(const SE_ImageDataID& imageDataID)
    {
        mImageDataID = imageDataID;
    }
    int getUnitWidth()
    {
        return mUnitWidth;
    }
    void setUnitWidth(int w)
    {
        mUnitWidth = w;
    }
    int getUnitHeight()
    {
        return mUnitHeight;
    }
    void setUnitHeight(int h)
    {
        mUnitHeight = h;
    }
private:
    SE_StringID mID;
    SE_ImageDataID mImageDataID;
    int mUnitWidth;
    int mUnitHeight;
    int mMirrorType;
    SE_ObjectManager<SE_StringID, SE_ImageRect> mImageRectSet;
};
typedef SE_TableSet<SE_StringID, SE_ImageMap> SE_ImageMapSet;
typedef SE_TableManager<SE_StringID, SE_ImageMapSet> SE_ImageMapManager;
#endif
