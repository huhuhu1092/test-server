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
    SE_DataItem di(SE_DataItem::CHAR_ITEM);
    di.data.c = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned char v, int index)
{
    SE_DataItem di(SE_DataItem::UCHAR_ITEM);
    di.data.uc = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(short v, int index)
{
    SE_DataItem di(SE_DataItem::SHORT_ITEM);
    di.data.s = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned short v, int index)
{
    SE_DataItem di(SE_DataItem::USHORT_ITEM);
    di.data.us = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(int v, int index)
{
    SE_DataItem di(SE_DataItem::INT_ITEM);
    di.data.i = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(unsigned int v, int index)
{
    SE_DataItem di(SE_DataItem::UINT_ITEM);
    di.data.ui = v;
    SET_DATA_ITEM(di, index);
}

void SE_StructItem::setDataItem(float v, int index)
{
    SE_DataItem di(SE_DataItem::FLOAT_ITEM);
    di.data.f = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setAsciiDataItem(char* v, int index)
{
    SE_DataItem di(SE_DataItem::ASCII_ITEM);
    di.data.ascii = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setUtf8DataItem(char* v, int index)
{
    SE_DataItem di(SE_DataItem::UTF8_ITEM);
    di.data.utf8 = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setUnicodeDataItem(wchar_t* v, int index)
{
    SE_DataItem di(SE_DataItem::UNICODE_ITEM);
    di.data.unicode = v;
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector2f& v, int index)
{
    SE_DataItem di(SE_DataItem::VECTOR2F_ITEM);
    di.data.vec2f = new SE_Vector2f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector3f& v, int index)
{
    SE_DataItem di(SE_DataItem::VECTOR3F_ITEM);
    di.data.vec3f = new SE_Vector3f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector4f& v, int index)
{
    SE_DataItem di(SE_DataItem::VECTOR4F_ITEM);
    di.data.vec4f = new SE_Vector4f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Vector3i v, int index)
{
    SE_DataItem di(SE_DataItem::VECTOR3I_ITEM);
    di.data.vec3i = new SE_Vector3i(v);
    SET_DATA_ITEM(di, index);    
}
void SE_StructItem::setDataItem(const SE_Quat& v, int index)
{
    SE_DataItem di(SE_DataItem::QUAT_ITEM);
    di.data.quat = new SE_Quat(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix2f& v, int index)
{
    SE_DataItem di(SE_DataItem::MATRIX2F_ITEM);
    di.data.mat2f = new SE_Matrix2f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix3f& v, int index)
{
    SE_DataItem di(SE_DataItem::MATRIX3F_ITEM);
    di.data.mat3f = new SE_Matrix3f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(const SE_Matrix4f& v, int index)
{
    SE_DataItem di(SE_DataItem::MATRIX4F_ITEM);
    di.data.mat4f = new SE_Matrix4f(v);
    SET_DATA_ITEM(di, index);
}
void SE_StructItem::setDataItem(SE_VirtualData* data, int index)
{
	SE_DataItem di(SE_DataItem::VIRTUALDATA_ITEM);
	di.data.virtualData = data;
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
