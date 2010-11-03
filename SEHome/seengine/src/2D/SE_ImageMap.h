#ifndef SE_IMAGEMAP_H
#define SE_IMAGEMAP_H
#include "SE_ID.h"
#include "SE_ObjectManager.h"
#include "SE_TableManager.h"
struct SE_ImageUnit
{
	enum {NO_MIRROR, MIRROR_X, MIRROR_Y, MIRROR_XY};
    int x;
    int y;
    int pivotx;
    int pivoty;
    int width;
    int height;
    int mirrorType;
	SE_ImageDataID imageDataID;
    SE_ImageUnit()
    {
        x = y = 0;
        pivotx = pivoty = 0;
        width = height = -1;
		mirrorType = NO_MIRROR;
    }
};

class SE_ImageMapProperty
{
public:
    SE_ImageMapProperty()
    {
        mUnitWidth = -1;
        mUnitHeight = -1;
    }
    void setID(const SE_StringID& id)
    {
        mID = id;
    }
    SE_StringID getID()
    {
        return mID;
    }
    SE_StringID getImageRef()
    {
        return mImageRef;
    }
    void setImageRef(const SE_StringID& imageref)
    {
        mImageRef = imageref;
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
    SE_StringID mImageRef;
    int mUnitWidth;
    int mUnitHeight;
    //SE_ObjectManager<SE_StringID, SE_ImageUnit> mImageRectSet;
};
typedef SE_Table<SE_StringID, SE_ImageUnit, SE_ImageMapProperty> SE_ImageMap;
typedef SE_Table<SE_StringID, SE_ImageMap*> SE_ImageMapSet;
typedef SE_Table<SE_StringID, SE_ImageMapSet*> SE_ImageMapManager;
#endif
