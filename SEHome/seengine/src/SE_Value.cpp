#include "SE_Value.h"
SE_Value::SE_Value(SE_Value::TYPE type)
{
    this->type = type;
    memset(&data, 0, sizeof(_Data));
}
SE_Value::~SE_VALUE()
{
    switch(type)
    {
    case ASCII:
        delete[] data.ascii;
        break;
    case UTF8:
        delete[] data.utf8;
        break;
    case UNICODE:
        delete[] data.unicode;
        break;
    case VECTOR3F:
        delete data.vec3f;
        break;
    case VECTOR4F:
        delete data.vec4f;
        break;
    case VECTOR2F:
        delete data.vec2f;
        break;
    case VECTOR3I:
        delete data.vec3i;
        break;
    case QUAT:
        delete data.quat;
        break;
    case MATRIX3F:
        delete data.mat3f;
        break;
    case MATRIX2F:
        delete data.mat2f;
        break;
    case MATRIX4F:
        delete data.mat4f;
        break;
    case VIRTUALDATA:
        delete data.virtualData;
        break;
    }
}
SE_Value::SE_Value(const SE_Value& right)
{
    type = right.type;
    switch(type)
    {
    case CHAR:
        data.c = right.data.c;
        break;
    case UCHAR:
        data.uc = right.data.uc;
        break;
    case SHORT:
        data.s = right.data.s;
        break;
    case USHORT:
        data.us = right.data.us;
        break;
    case INT:
        data.i = right.data.i;
        break;
    case UINT:
        data.ui = right.data.ui;
        break;
    case FLOAT:
        data.f = right.data.f;
        break;
    case ASCII:
        if(right.data.ascii)
        {
            size_t len = strlen(right.data.ascii);
            data.ascii = new char[len + 1];
            memset(data.ascii, 0, len + 1);
            strncpy(data.ascii, right.data.ascii, len);
        }
        break;
    case UTF8:
        if(right.data.utf8)
        {
            size_t len = strlen(right.data.utf8);
            data.utf8 = new char[len + 1];
            memset(data.utf8, 0, len + 1);
            strncpy(data.utf8, right.data.utf8, len);
        }
        break;
    case UNICODE:
        if(right.data.unicode)
        {
            size_t len = wcslen(right.data.unicode);
            data.unicode = new wchar_t[len + 1];
            memset(data.unicode, 0, sizeof(wchar_t) * (len + 1));
            wcscpy(data.unicode, right.data.unicode);
        }
        break;
    case VECTOR3F:
        data.vec3f = new SE_Vector3f;
        *data.vec3f = *right.data.vec3f;
        break;
    case VECTOR4F:
        data.vec4f = new SE_Vector4f;
        *data.vec4f = *right.data.vec4f;
        break;
    case VECTOR2F:
        data.vec2f = new SE_Vector2f;
        *data.vec2f = *right.data.vec2f;
        break;
    case VECTOR3I:
        data.vec3i = new SE_Vector3i;
        *data.vec3i = *right.data.vec3i;
        break;
    case QUAT:
        data.quat = new SE_Quat;
        *data.quat = *right.data.quat;
        break;
    case MATRIX3F:
        data.mat3f = new SE_Matrix3f;
        *data.mat3f = *right.data.mat3f;
        break;
    case MATRIX2F:
        data.mat2f = new SE_Matrix2f;
        *data.mat2f = *right.data.mat2f;
        break;
    case MATRIX4F:
        data.mat4f = new SE_Matrix4f;
        *data.mat4f = *right.data.mat4f;
        break;
    case VIRTUALDATA:
        data.virtualData = right.data.virtualData->clone();
        break;
    }
}
SE_Value& SE_DataItem::operator=(const SE_Value& right)
{
    if(this == &right)
        return *this;
    switch(type)
    {
    case CHAR:
        break;
    case UCHAR:
        break;
    case SHORT:
        break;
    case USHORT:
        break;
    case INT:
        break;
    case UINT:
        break;
    case FLOAT:
        break;
    case ASCII:
        if(data.ascii)
            delete[] data.ascii;
        break;
    case UTF8:
        if(data.utf8)
            delete[] data.utf8;
        break;
    case UNICODE:
        if(data.unicode)
            delete[] data.unicode;
        break;
    case VECTOR3F:
        if(data.vec3f)
            delete data.vec3f;
        break;
    case VECTOR4F:
        if(data.vec4f)
            delete data.vec4f;
        break;
    case VECTOR2F:
        if(data.vec2f)
            delete data.vec2f;
        break;
    case VECTOR3I:
        if(data.vec3i)
            delete data.vec3i;
        break;
    case QUAT:
        if(data.quat)
            delete data.quat;
        break;
    case MATRIX3F:
        if(data.mat3f)
            delete data.mat3f;
        break;
    case MATRIX2F:
        if(data.mat2f)
            delete data.mat2f;
        break;
    case MATRIX4F:
        if(data.mat4f)
            delete data.mat4f;
        break;
    case VIRTUALDATA:
        if(data.virtualData)
            delete data.virtualData;
        break;
    }
    type = right.type;
    switch(type)
    {
    case CHAR:
        data.c = right.data.c;
        break;
    case UCHAR:
        data.uc = right.data.uc;
        break;
    case SHORT:
        data.s = right.data.s;
        break;
    case USHORT:
        data.us = right.data.us;
        break;
    case INT:
        data.i = right.data.i;
        break;
    case UINT:
        data.ui = right.data.ui;
        break;
    case FLOAT:
        data.f = right.data.f;
        break;
    case ASCII:
        if(right.data.ascii)
        {
            int len = strlen(right.data.ascii);
            data.ascii = new char[len + 1];
            memset(data.ascii, 0, len + 1);
            strncpy(data.ascii, right.data.ascii, len);
        }
        break;
    case UTF8:
        if(right.data.utf8)
        {
            int len = strlen(right.data.utf8);
            data.utf8 = new char[len + 1];
            memset(data.utf8, 0, len + 1);
            strncpy(data.utf8, right.data.utf8, len);
        }
        break;
    case UNICODE:
        if(right.data.unicode)
        {
            size_t len = wcslen(right.data.unicode);
            data.unicode = new wchar_t[len + 1];
            memset(data.unicode, 0, sizeof(wchar_t) * (len + 1));
            wcscpy(data.unicode, right.data.unicode);
        }
        break;
    case VECTOR3F:
        data.vec3f = new SE_Vector3f;
        *data.vec3f = *right.data.vec3f;
        break;
    case VECTOR4F:
        data.vec4f = new SE_Vector4f;
        *data.vec4f = *right.data.vec4f;
        break;
    case VECTOR2F:
        data.vec2f = new SE_Vector2f;
        *data.vec2f = *right.data.vec2f;
        break;
    case VECTOR3I:
        data.vec3i = new SE_Vector3i;
        *data.vec3i = *right.data.vec3i;
        break;
    case QUAT:
        data.quat = new SE_Quat;
        *data.quat = *right.data.quat;
        break;
    case MATRIX3F:
        data.mat3f = new SE_Matrix3f;
        *data.mat3f = *right.data.mat3f;
        break;
    case MATRIX2F:
        data.mat2f = new SE_Matrix2f;
        *data.mat2f = *right.data.mat2f;
        break;
    case MATRIX4F:
        data.mat4f = new SE_Matrix4f;
        *data.mat4f = *right.data.mat4f;
        break;
    case VIRTUALDATA:
        data.virtualData = right.data.virtualData->clone();
        break;
    }
    return *this;
}
bool SE_Value::operator==(const SE_Value& right)
{
    if(type != right.type)
        return false;
    bool ret = false;
    switch(type)
    {
    case CHAR:
        ret = data.c == right.data.c;
        break;
    case UCHAR:
        ret = data.uc == right.data.uc;
        break;
    case SHORT:
        ret = data.s == right.data.s;
        break;
    case USHORT:
        ret = data.us == right.data.us;
        break;
    case INT:
        ret = data.i == right.data.i;
        break;
    case UINT:
        ret = data.ui == right.data.ui;
        break;
    case FLOAT:
        ret = data.f == right.data.f;
        break;
    case ASCII:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case UTF8:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case UNICODE:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case VECTOR3F:
        ret = *data.vec3f == *right.data.vec3f;
        break;
    case VECTOR4F:
        ret = *data.vec4f == *right.data.vec4f;
        break;
    case VECTOR2F:
        ret = *data.vec2f == *right.data.vec2f;
        break;
    case VECTOR3I:
        ret = *data.vec3i == *right.data.vec3i;
        break;
    case QUAT:
        ret = *data.quat == *right.data.quat;
        break;
    case MATRIX3F:
        ret = *data.matrix3f == *right.data.mat3f;
        break;
    case MATRIX2F:
        ret = *data.matrix2f == *right.data.mat2f;
        break;
    case MATRIX4F:
        ret = *data.mat4f == *right.data.mat4f;
        break;
    case VIRTUALDATA:
        ret = data.virtualData->eq(right.data.virtualData);
        break;
    }
    return ret;
}
bool SE_Value::operator!=(const SE_Value& right)
{
    return !this->operator==(right);
}
    void SE_Value::setAscii(const char* v)
    {
        type = ASCII;
        size_t len = strlen(v);
        char* ascii = new char[len + 1];
        if(!ascii)
            return;
        memset(ascii, 0, len + 1);
        strncpy(ascii, v, len);
        if(data.ascii)
            delete[] data.ascii;
        data.ascii = ascii;
    }
