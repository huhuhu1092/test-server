#include "SE_Struct.h"
typedef SE_Value SE_DataItem;
SE_DataItemGroup::SE_DataItemGroup(int size)
{
    mDataItems.resize(size);
}
int SE_DataItemGroup::getDataItemCount()
{
    return mDataItems.size();
}
SE_Value SE_DataItemGroup::getDataItem(int i)
{
    if(i < 0 || i >= mDataItems.size())
        return SE_Value();
    return mDataItems[i];
}
void SE_DataItemGroup::setDataItem(int i , const SE_Value& di)
{
    if(i < 0 || i >= mDataItems.size())
        return ;
    mDataItems[i] = di;  
}
/////////
SE_StructItem::SE_StructItem(int dataItemCount) : mDataItemGroup(dataItemCount)
{
    
}
SE_StructItem::~SE_StructItem()
{
    /*
    switch(type)
    {
    case DATA_ITEM:
        delete data.item;
        break;
    case DATA_GROUP:
        delete data.group;
        break;
    }
    */
}
/*
SE_StructItem::SE_StructItem(const SE_StructItem& right)
{

    
}
SE_StructItem& SE_StructItem::operator=(const SE_StructItem&)
{

}
*/
#define SET_DATA_ITEM(di, index) \
{\
    mDataItemGroup.setDataItem(index, di); \
}
void SE_StructItem::setDataItem(char v, int index)
{
    SE_DataItem di;
	di.setChar(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned char v, int index)
{
    SE_DataItem di;
	di.setUchar(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(short v, int index)
{
    SE_DataItem di;
	di.setShort(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned short v, int index)
{
    SE_DataItem di;
	di.setUShort(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(int v, int index)
{
    SE_DataItem di;
	di.setInt(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned int v, int index)
{
    SE_DataItem di;
	di.setUint(v);
    SET_DATA_ITEM(di, index);
}

void SE_StructItem::setDataItem(float v, int index)
{
    SE_DataItem di;
	di.setFloat(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setAsciiDataItem(char* v, int index)
{
    SE_DataItem di;
	di.setAscii(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setUtf8DataItem(char* v, int index)
{
    SE_DataItem di;
	di.setUtf8(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setUnicodeDataItem(wchar_t* v, int index)
{
    SE_DataItem di;
	di.setUnicode(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector2f& v, int index)
{
    SE_DataItem di;
	di.setVector2f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector3f& v, int index)
{
    SE_DataItem di;
	di.setVector3f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector4f& v, int index)
{
    SE_DataItem di;
	di.setVector4f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector3i v, int index)
{
    SE_DataItem di;
	di.setVector3i(v);
    SET_DATA_ITEM(di, index);    
}
void SE_StructItem::setDataItem(const SE_Quat& v, int index)
{
    SE_DataItem di;
    di.setQuat(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix2f& v, int index)
{
    SE_DataItem di;
	di.setMatrix2f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix3f& v, int index)
{
    SE_DataItem di;
	di.setMatrix3f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix4f& v, int index)
{
    SE_DataItem di;
	di.setMatrix4f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(SE_VirtualData* data, int index)
{
	SE_DataItem di;
	di.setVirtualData(data);
	SET_DATA_ITEM(di, index);
}
////////////////////////////
SE_Struct::SE_Struct(int structItemSize) : mStructItems(structItemSize, (SE_StructItem *)NULL)
{
}
SE_Struct::~SE_Struct()
{
    for(int i = 0 ; i < mStructItems.size() ; i++)
    {
        SE_StructItem* item = mStructItems[i];
        if(item)
            delete item;
    }
    
}
int SE_Struct::getCount()
{
    return mStructItems.size();
}
SE_StructItem* SE_Struct::getStructItem(int index)
{
    if(index < 0 || index >= mStructItems.size())
        return NULL;
    return mStructItems[index];
}
void SE_Struct::setStructItem(int index, SE_StructItem* si)
{
    if(index < 0 || index >= mStructItems.size())
        return;
    SE_StructItem* item = mStructItems[index];
    if(item)
        delete item;
    mStructItems[index] = si;
}
