#ifndef SE_VALUE_H
#define SE_VALUE_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include <wchar.h>
class SE_VirtualData
{
public:
	virtual ~SE_VirtualData() {};
    virtual SE_VirtualData* clone() = 0;
    virtual bool eq(const SE_VirtualData& right) = 0;
    virtual bool neq(const SE_VirtualData& right) = 0;
};

class SE_Value
{
public:
    enum TYPE {INVALID, CHAR, UCHAR, SHORT, USHORT, INT, UINT, FLOAT,ASCII, UTF8, UNICODE,
         VECTOR3F, VECTOR4F, VECTOR2F, VECTOR3I, QUAT, MATRIX3F, MATRIX2F, MATRIX4F, VIRTUALDATA};
    SE_Value(TYPE type = INVALID);
    ~SE_Value();
    SE_Value(const SE_Value&);
    SE_Valuem& operator=(const SE_Value&);
    bool operator==(const SE_Value& right);
    bool operator!=(const SE_Value& right);
    int getInt() const
    {
        return data.i;
    }
    void setInt(int v)
    {
        type = INT;
        data.i = v;
    }
    unsigned int getUint() const
    {
        return data.ui;
    }
    void setUint(unsigned int v)
    {
        type = UINT;
        data.ui = v;
    }
    char getChar() const
    {
        return data.c;
    }
    void setChar(char v)
    {
        type = CHAR;
        data.c = v;
    }
    unsigned char getUchar() const
    {
        return data.uc;
    }
    void setUchar(unsigned char v)
    {
        type = UCHAR;
        data.uc = v;
    }
    float getFloat() const
    {
        return data.f;
    }
    void setFloat(float v)
    {
        type = FLOAT;
        data.f = v;
    }
    const char* getAscii() const
    {
        return data.ascii;
    }
    void setAscii(const char* v);
    const char* getUtf8() const
    {
        return data.utf8;
    }
    void setUtf8(const char* v)
    {
        type = UTF8;
        setAscii(v);
    }
    const wchar_t* getUnicode() const
    {
        return data.unicode;
    }
    void setUnicode(const wchar_t* v)
    {
        type = UNICODE;
        setAscii((const char*)v);
    }
    const SE_Vector3f& getVector3f() const
    {
        return *data.vec3f;
    }
    void setVector3f(const SE_Vector3f& v)
    {
        type = VECTOR3F;
        if(data.vec3f)
            delete data.vec3f;
        data.vec3f = new SE_Vector3f(v);
    }
    const SE_Vector2f& getVector2f() const
    {
        return *data.vec2f;
    }
    void setVector2f(const SE_Vector2f& v)
    {
        type = VECTOR2F;
        if(data.vec2f)
            delete data.vec2f;
        data.vec2f = new SE_Vector2f(v);
    }
    const SE_Vector4f& getVector4f() const
    {
        return *data.vec4f;
    }
    void setVector4f(const SE_Vector4f& v)
    {
        type = VECTOR4F;
        if(data.vec4f)
            delete data.vec4f;
        data.vec4f = new SE_Vector4f(v);
    }
    const SE_Vector3i& getVector3i() const
    {
        return *data.vec3i;
    }
    void setVector3i(const SE_Vector3i& v)
    {
        type = VECTOR3I;
        if(data.vec3i)
            delete data.vec3i;
        data.vec3i = new SE_Vector3i(v);
    }
    const SE_Matrix2f& getMatrix2f() const
    {
        return *data.mat2f;
    }
    void setMatrix2f(const SE_Matrix2f& v)
    {
        type = MATRIX2F;
        if(data.mat2f)
            delete data.mat2f;
        data.mat2f = new SE_Matrix2f(v);
    }
    const SE_Matrix3f& getMatrix3f() const
    {
        return *data.mat3f;
    }
    void setMatrix3f(const SE_Matrix3f& v)
    {
        type = MATRIX3F;
        if(data.mat3f)
            delete data.mat3f;
        data.mat3f = new SE_Matrix3f(v);
    }
    const SE_Matrix4f& getMatrix4f() const
    {
        return *data.mat4f;
    }
    void setMatrix4f(const SE_Matrix4f& v)
    {
        type = MATRIX4F;
        if(data.mat4f)
            delete data.mat4f;
        data.mat4f = new SE_Matrix4f(v);
    }
    void setVirtualData(SE_VirtualData* d)
    {
        type = VIRTUAL_DATA;
        if(data.virtualData)
            delete data.virtualData;
        data.virtualData = d;
    }
    SE_VirtualData* getVirtualData() const
    {
        return data.virtualData;
    }
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
};

#endif
