#ifndef SE_IMAGEMAP_H
#define SE_IMAGEMAP_H
#include "SE_ID.h"
#include "SE_TableManager.h"
struct SE_ImageRect
{
	enum {NO_MIRROR, MIRROR_X, MIRROR_Y, MIRROR_XY};
    int x;
    int y;
    int width;
    int height;
    int mirrorType;
	SE_ImageRect()
    {
        x = y = 0;
        width = height = -1;
		mirrorType = NO_MIRROR;
    }
};
struct SE_ImageUnit
{
    SE_ImageRect imageRect;
	SE_ImageDataID imageDataID;
	SE_StringID imageURL; // when you get a SE_ImageUnit From SE_ResourceManager, it will 
	                      // separate your input url to imageURL and ext.
	SE_StringID ext;
	bool isValid()
	{
		return imageDataID.isValid() && imageRect.width > 0 && imageRect.height > 0;
	}

};

class SE_ImageItemProperty
{
public:
    void setImageDataID(const SE_ImageDataID& id)
    {
        mImageDataID = id;
    }
    SE_ImageDataID getImageDataID()
    {
        return mImageDataID;
    }
private:
    SE_ImageDataID mImageDataID;
    //SE_ObjectManager<SE_StringID, SE_ImageRect> mImageRectSet;
};
typedef SE_Table<SE_StringID, SE_ImageRect, SE_ImageItemProperty> SE_ImageItem;
typedef SE_Table<SE_StringID, SE_ImageItem*> SE_ImageMap;
typedef SE_Table<SE_StringID, SE_ImageMap*> SE_ImageMapSet;
typedef SE_Table<SE_StringID, SE_ImageMapSet*> SE_ImageTable;
#endif
