#ifndef SE_VALUE_H
#define SE_VALUE_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Log.h"
#include <wchar.h>
class SE_VirtualData
{
public:
    SE_VirtualData()
    {
        mDeletedAuto = true;
    }
	virtual ~SE_VirtualData() {};
    virtual SE_VirtualData* clone() = 0;
    virtual bool eq(const SE_VirtualData& right) = 0;
    virtual bool neq(const SE_VirtualData& right) = 0;
    void setAutoDeleted(bool v)
    {
        mDeletedAuto = v;
    }
    bool isAutoDeleted()
    {
        return mDeletedAuto;
    }
private:
    bool mDeletedAuto;
};
#define SE_PRINT_TYPEERROR_MSG(type) LOGI("## error: value type is not "#type" ##\n");
class SE_Value
{
public:
    enum TYPE {INVALID, CHAR_T, UCHAR_T, SHORT_T, USHORT_T, INT_T, UINT_T, FLOAT_T, ASCII_T, UTF8_T, UNICODE_T, \
         VECTOR3F_T, VECTOR4F_T, VECTOR2F_T, VECTOR3I_T, QUAT_T, MATRIX3F_T, MATRIX2F_T, MATRIX4F_T, VIRTUALDATA_T};
	SE_Value();
	bool hasSet() const
	{
        return mHasSetValue;
	}
    ~SE_Value();
    SE_Value(const SE_Value&);
    SE_Value& operator=(const SE_Value&);
    bool operator==(const SE_Value& right);
    bool operator!=(const SE_Value& right);
	TYPE getType()
	{
		return type;
	}
    int getInt() const
    {
		SE_ASSERT(type == INT_T);
		if(type != INT_T)
		{
			SE_PRINT_TYPEERROR_MSG(INT_T);
			return 0xFFFFFFFF;
		}
        return data.i;
    }
    void setInt(int v)
    {
		if(mHasSetValue)
			return;
        type = INT_T;
        data.i = v;
		mHasSetValue = true;
    }
    void setUShort(unsigned short us)
    {
		if(mHasSetValue)
			return;
        type = USHORT_T;
        data.us = us;
		mHasSetValue = true;
    }
    void setShort(short s)
    {
		if(mHasSetValue)
			return;
        type = SHORT_T;
        data.s = s;
		mHasSetValue = true;
    }
    unsigned short getUShort() const
    {
		SE_ASSERT(type == USHORT_T);
		if(type != USHORT_T)
		{
			SE_PRINT_TYPEERROR_MSG(USHORT_T);
		}
        return data.us;
    } 
    short getShort() const
    {
		SE_ASSERT(type == SHORT_T);
		if(type != SHORT_T)
		{
			SE_PRINT_TYPEERROR_MSG(SHORT_T);
		}
        return data.s;
    }

    unsigned int getUint() const
    {
		SE_ASSERT(type == UINT_T);
		if(type != UINT_T)
		{
			SE_PRINT_TYPEERROR_MSG(UINT_T);
		}
        return data.ui;
    }
    void setUint(unsigned int v)
    {
		if(mHasSetValue)
			return;
        type = UINT_T;
        data.ui = v;
		mHasSetValue = true;
    }
    char getChar() const
    {
		SE_ASSERT(type == CHAR_T);
		if(type != CHAR_T)
		{
			SE_PRINT_TYPEERROR_MSG(CHAR_T);
		}
        return data.c;
    }
    void setChar(char v)
    {
		if(mHasSetValue)
			return;
        type = CHAR_T;
        data.c = v;
		mHasSetValue = true;
    }
    unsigned char getUchar() const
    {
		SE_ASSERT(type == UCHAR_T);
		if(type != UCHAR_T)
		{
			SE_PRINT_TYPEERROR_MSG(UCHAR_T);
		}
        return data.uc;
    }
    void setUchar(unsigned char v)
    {
		if(mHasSetValue)
			return;
        type = UCHAR_T;
        data.uc = v;
		mHasSetValue = true;;
    }
    float getFloat() const
    {
		SE_ASSERT(type == FLOAT_T);
		if(type != FLOAT_T)
		{
			SE_PRINT_TYPEERROR_MSG(FLOAT_T);
		}
        return data.f;
    }
    void setFloat(float v)
    {
		if(mHasSetValue)
			return;
        type = FLOAT_T;
        data.f = v;
		mHasSetValue = true;
    }
    const char* getAscii() const
    {
		SE_ASSERT(type == ASCII_T);
		if(type != ASCII_T)
		{
			SE_PRINT_TYPEERROR_MSG(ASCII_T);
		}
        return data.ascii;
    }
    void setAscii(const char* v)
	{
		if(mHasSetValue)
			return;
		type = ASCII_T;
		SE_ASSERT(data.ascii == NULL);
		data.ascii = copyString(v);
		mHasSetValue = true;
	}
    const char* getUtf8() const
    {
		SE_ASSERT(type == UTF8_T);
		if(type != UTF8_T)
		{
			SE_PRINT_TYPEERROR_MSG(UTF8_T);
		}
        return data.utf8;
    }
    void setUtf8(const char* v)
    {
		if(mHasSetValue)
			return;
        type = UTF8_T;
		SE_ASSERT(data.utf8 == NULL);
        data.utf8 = copyString(v);
		mHasSetValue = true;
    }
    const wchar_t* getUnicode() const
    {
		SE_ASSERT(type == UNICODE_T);
		if(type != UNICODE_T)
		{
			SE_PRINT_TYPEERROR_MSG(UNICODE_T);
		}
        return data.unicode;
    }
    void setUnicode(const wchar_t* v)
    {
		if(mHasSetValue)
			return;
        type = UNICODE_T;
		SE_ASSERT(data.unicode == NULL);
        data.unicode = (wchar_t*)copyString((const char*)v);
		mHasSetValue = true;
    }
	const SE_Quat& getQuat() const
	{
		SE_ASSERT(type == QUAT_T);
		if(type != QUAT_T)
		{
			SE_PRINT_TYPEERROR_MSG(QUAT_T);
		}
		return *data.quat;
	}
	void setQuat(const SE_Quat& q)
	{
		if(mHasSetValue)
			return;
		SE_ASSERT(data.quat == NULL);
		data.quat = new SE_Quat(q);
		mHasSetValue = true;
	}
    const SE_Vector3f& getVector3f() const
    {
		SE_ASSERT(type == VECTOR3F_T);
		if(type != VECTOR3F_T)
		{
			SE_PRINT_TYPEERROR_MSG(VECTOR3F_T);
		}
        return *data.vec3f;
    }
    void setVector3f(const SE_Vector3f& v)
    {
		if(mHasSetValue)
			return;
        type = VECTOR3F_T;
		SE_ASSERT(data.vec3f == NULL);
        data.vec3f = new SE_Vector3f(v);
		mHasSetValue = true;
    }
    const SE_Vector2f& getVector2f() const
    {
		SE_ASSERT(type != VECTOR2F_T);
		if(type != VECTOR2F_T)
		{
			SE_PRINT_TYPEERROR_MSG(VECTOR2F_T);
		}
        return *data.vec2f;
    }
    void setVector2f(const SE_Vector2f& v)
    {
		if(mHasSetValue)
			return;
        type = VECTOR2F_T;
        SE_ASSERT(data.vec2f == NULL);
        data.vec2f = new SE_Vector2f(v);
		mHasSetValue = true;
    }
    const SE_Vector4f& getVector4f() const
    {
		SE_ASSERT(type == VECTOR4F_T);
		if(type != VECTOR4F_T)
		{
			SE_PRINT_TYPEERROR_MSG(VECTOR4F_T);
		}
        return *data.vec4f;
    }
    void setVector4f(const SE_Vector4f& v)
    {
		if(mHasSetValue)
			return;
        type = VECTOR4F_T;
        SE_ASSERT(data.vec4f == NULL);
        data.vec4f = new SE_Vector4f(v);
		mHasSetValue = true;
    }
    const SE_Vector3i& getVector3i() const
    {
		SE_ASSERT(type == VECTOR3I_T);
		if(type != VECTOR3I_T)
		{
			SE_PRINT_TYPEERROR_MSG(VECTOR3I_T);
		}
        return *data.vec3i;
    }
    void setVector3i(const SE_Vector3i& v)
    {
		if(mHasSetValue)
			return;
        type = VECTOR3I_T;
        SE_ASSERT(data.vec3f == NULL);
        data.vec3i = new SE_Vector3i(v);
		mHasSetValue = true;
    }
    const SE_Matrix2f& getMatrix2f() const
    {
		SE_ASSERT(type == MATRIX2F_T);
		if(type != MATRIX2F_T)
		{
			SE_PRINT_TYPEERROR_MSG(MATRIX2F_T);
		}
        return *data.mat2f;
    }
    void setMatrix2f(const SE_Matrix2f& v)
    {
		if(mHasSetValue)
			return;
        type = MATRIX2F_T;
        SE_ASSERT(data.mat2f == NULL);
        data.mat2f = new SE_Matrix2f(v);
		mHasSetValue = true;
    }
    const SE_Matrix3f& getMatrix3f() const
    {
		SE_ASSERT(type == MATRIX3F_T);
		if(type != MATRIX3F_T)
		{
			SE_PRINT_TYPEERROR_MSG(MATRIX3F_T);
		}
		
        return *data.mat3f;
    }
    void setMatrix3f(const SE_Matrix3f& v)
    {
		if(mHasSetValue)
			return;
        type = MATRIX3F_T;
        SE_ASSERT(data.mat3f == NULL);
        data.mat3f = new SE_Matrix3f(v);
		mHasSetValue = true;
    }
    const SE_Matrix4f& getMatrix4f() const
    {
		SE_ASSERT(type == MATRIX4F_T);
		if(type != MATRIX4F_T)
		{
			SE_PRINT_TYPEERROR_MSG(MATRIX4F_T);
		}
        return *data.mat4f;
    }
    void setMatrix4f(const SE_Matrix4f& v)
    {
		if(mHasSetValue)
			return;
        type = MATRIX4F_T;
        SE_ASSERT(data.mat4f == NULL);
        data.mat4f = new SE_Matrix4f(v);
		mHasSetValue = true;
    }
    void setVirtualData(SE_VirtualData* d)
    {
		if(mHasSetValue)
			return;
		if(d == NULL)
			return;
        type = VIRTUALDATA_T;
        SE_ASSERT(data.virtualData == NULL);
        data.virtualData = d;
		mHasSetValue = true;
    }
    SE_VirtualData* getVirtualData() const
    {
		SE_ASSERT(type == VIRTUALDATA_T);
		if(type != VIRTUALDATA_T)
		{
			SE_PRINT_TYPEERROR_MSG(VIRTUALDATA_T);
		}
        return data.virtualData;
    }
private:
    char* copyString(const char* v);
	void clear();
	void setRight(const SE_Value& right);
private:
    union _Data
    {
        char c;
        unsigned char uc;
        short s;
        unsigned short us;
        int i;
        unsigned int ui;
        float f;
        char* ascii;
        char* utf8;
        wchar_t* unicode;
        SE_Vector3f* vec3f;
        SE_Vector2f* vec2f;
        SE_Vector4f* vec4f;
        SE_Vector3i* vec3i;
        SE_Quat* quat;
        SE_Matrix2f* mat2f;
        SE_Matrix3f* mat3f;
        SE_Matrix4f* mat4f;
        SE_VirtualData* virtualData;
    } data;
    TYPE type;
	bool mHasSetValue;
	friend class SE_StructItem;
};

#endif
