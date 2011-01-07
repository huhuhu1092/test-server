#ifndef SE_STRUCT_H
#define SE_STRUCT_H
#include <wchar.h>
#include <vector>
#include <string>
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Matrix.h"
#include "SE_Value.h"
class SE_Spatial;

class SE_StdString : public SE_VirtualData
{
public:
	~SE_StdString() {};
	SE_VirtualData* clone()
	{
		SE_StdString* str = new SE_StdString;
		str->data = data;
		return str;
	}
	bool eq(const SE_VirtualData& right)
	{
		const SE_StdString& v = (const SE_StdString&)right;
		return data == v.data;
	}
	bool neq(const SE_VirtualData& right)
	{
		const SE_StdString& v = (const SE_StdString&)right;
		return data != v.data;
	}
	std::string data;
};
class SE_SpatialData : public SE_VirtualData
{
public:
	SE_VirtualData* clone()
	{
		SE_SpatialData* s = new SE_SpatialData;
		s->spatial = spatial;
		return s;
	}
	bool eq(const SE_VirtualData& right)
	{
		const SE_SpatialData& v = (const SE_SpatialData&)right;
		return v.spatial == spatial;
	}
	bool neq(const SE_VirtualData& right)
	{
		const SE_SpatialData& v = (const SE_SpatialData&)right;
		return v.spatial != spatial;
	}
	SE_Spatial* spatial;
};

class SE_DataItemGroup
{
public:
    SE_DataItemGroup(int size);
    int getDataItemCount();
    SE_Value getDataItem(int i);
    void setDataItem(int i , const SE_Value& di);
private:
    std::vector<SE_Value> mDataItems;
};
class  SE_StructItem
{
public:
    SE_StructItem(int dataItemCount);
    ~SE_StructItem();
    //SE_StructItem(const SE_StructItem&);
    //SE_StructItem& operator=(const SE_StructItem&);
    void setDataItem(char v, int index = 0);
    void setDataItem(unsigned char v, int index = 0);
    void setDataItem(short v, int index = 0);
    void setDataItem(unsigned short v, int index = 0);
    void setDataItem(int v, int index = 0);
    void setDataItem(unsigned int v, int index = 0);
    void setDataItem(float v, int index = 0);
    //v will be owned by SE_StructItem
    //it will be deleted by ~SE_StructItem
    void setAsciiDataItem(char* v, int index = 0);
    void setUtf8DataItem(char* v, int index = 0);
    void setUnicodeDataItem(wchar_t* v, int index = 0);
    void setDataItem(const SE_Vector2f& v, int index = 0);
    void setDataItem(const SE_Vector3f& v, int index = 0);
    void setDataItem(const SE_Vector4f& v, int index = 0);
    void setDataItem(const SE_Vector3i v, int index = 0);
    void setDataItem(const SE_Quat& v, int index = 0);
    void setDataItem(const SE_Matrix2f& v, int index = 0);
    void setDataItem(const SE_Matrix3f& v, int index = 0);
    void setDataItem(const SE_Matrix4f& v, int index = 0);
	void setDataItem(SE_VirtualData* data, int index = 0);
	SE_Value getDataItem(int index)
	{
		return mDataItemGroup.getDataItem(index);
	}
	int getDataItemCount()
	{
		return mDataItemGroup.getDataItemCount();
	}
private:
    SE_DataItemGroup mDataItemGroup;
};
class SE_Struct
{
public:
    SE_Struct(int structItemSize);
    ~SE_Struct();
    int getCount();
    SE_StructItem* getStructItem(int index);
    void setStructItem(int index, SE_StructItem* si);
private:
    SE_Struct(const SE_Struct& );
    SE_Struct& operator=(const SE_Struct&);
private:
    std::vector<SE_StructItem*> mStructItems;
};
#endif
