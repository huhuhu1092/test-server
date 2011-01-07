#include "SE_Value.h"
SE_Value::SE_Value(SE_Value::TYPE type)
{
    this->type = type;
    memset(&data, 0, sizeof(_Data));
}
SE_Value::~SE_Value()
{
    switch(type)
    {
    case ASCII_T:
        delete[] data.ascii;
        break;
    case UTF8_T:
        delete[] data.utf8;
        break;
    case UNICODE_T:
        delete[] data.unicode;
        break;
    case VECTOR3F_T:
        delete data.vec3f;
        break;
    case VECTOR4F_T:
        delete data.vec4f;
        break;
    case VECTOR2F_T:
        delete data.vec2f;
        break;
    case VECTOR3I_T:
        delete data.vec3i;
        break;
    case QUAT_T:
        delete data.quat;
        break;
    case MATRIX3F_T:
        delete data.mat3f;
        break;
    case MATRIX2F_T:
        delete data.mat2f;
        break;
    case MATRIX4F_T:
        delete data.mat4f;
        break;
    case VIRTUALDATA_T:
        delete data.virtualData;
        break;
    }
}
SE_Value::SE_Value(const SE_Value& right)
{
    type = right.type;
    switch(type)
    {
    case CHAR_T:
        data.c = right.data.c;
        break;
    case UCHAR_T:
        data.uc = right.data.uc;
        break;
    case SHORT_T:
        data.s = right.data.s;
        break;
    case USHORT_T:
        data.us = right.data.us;
        break;
    case INT_T:
        data.i = right.data.i;
        break;
    case UINT_T:
        data.ui = right.data.ui;
        break;
    case FLOAT_T:
        data.f = right.data.f;
        break;
    case ASCII_T:
        if(right.data.ascii)
        {
            size_t len = strlen(right.data.ascii);
            data.ascii = new char[len + 1];
            memset(data.ascii, 0, len + 1);
            strncpy(data.ascii, right.data.ascii, len);
        }
        break;
    case UTF8_T:
        if(right.data.utf8)
        {
            size_t len = strlen(right.data.utf8);
            data.utf8 = new char[len + 1];
            memset(data.utf8, 0, len + 1);
            strncpy(data.utf8, right.data.utf8, len);
        }
        break;
    case UNICODE_T:
        if(right.data.unicode)
        {
            size_t len = wcslen(right.data.unicode);
            data.unicode = new wchar_t[len + 1];
            memset(data.unicode, 0, sizeof(wchar_t) * (len + 1));
            wcscpy(data.unicode, right.data.unicode);
        }
        break;
    case VECTOR3F_T:
        data.vec3f = new SE_Vector3f;
        *data.vec3f = *right.data.vec3f;
        break;
    case VECTOR4F_T:
        data.vec4f = new SE_Vector4f;
        *data.vec4f = *right.data.vec4f;
        break;
    case VECTOR2F_T:
        data.vec2f = new SE_Vector2f;
        *data.vec2f = *right.data.vec2f;
        break;
    case VECTOR3I_T:
        data.vec3i = new SE_Vector3i;
        *data.vec3i = *right.data.vec3i;
        break;
    case QUAT_T:
        data.quat = new SE_Quat;
        *data.quat = *right.data.quat;
        break;
    case MATRIX3F_T:
        data.mat3f = new SE_Matrix3f;
        *data.mat3f = *right.data.mat3f;
        break;
    case MATRIX2F_T:
        data.mat2f = new SE_Matrix2f;
        *data.mat2f = *right.data.mat2f;
        break;
    case MATRIX4F_T:
        data.mat4f = new SE_Matrix4f;
        *data.mat4f = *right.data.mat4f;
        break;
    case VIRTUALDATA_T:
        data.virtualData = right.data.virtualData->clone();
        break;
    }
}
SE_Value& SE_Value::operator=(const SE_Value& right)
{
    if(this == &right)
        return *this;
    switch(type)
    {
    case CHAR_T:
        break;
    case UCHAR_T:
        break;
    case SHORT_T:
        break;
    case USHORT_T:
        break;
    case INT_T:
        break;
    case UINT_T:
        break;
    case FLOAT_T:
        break;
    case ASCII_T:
        if(data.ascii)
            delete[] data.ascii;
        break;
    case UTF8_T:
        if(data.utf8)
            delete[] data.utf8;
        break;
    case UNICODE_T:
        if(data.unicode)
            delete[] data.unicode;
        break;
    case VECTOR3F_T:
        if(data.vec3f)
            delete data.vec3f;
        break;
    case VECTOR4F_T:
        if(data.vec4f)
            delete data.vec4f;
        break;
    case VECTOR2F_T:
        if(data.vec2f)
            delete data.vec2f;
        break;
    case VECTOR3I_T:
        if(data.vec3i)
            delete data.vec3i;
        break;
    case QUAT_T:
        if(data.quat)
            delete data.quat;
        break;
    case MATRIX3F_T:
        if(data.mat3f)
            delete data.mat3f;
        break;
    case MATRIX2F_T:
        if(data.mat2f)
            delete data.mat2f;
        break;
    case MATRIX4F_T:
        if(data.mat4f)
            delete data.mat4f;
        break;
    case VIRTUALDATA_T:
        if(data.virtualData)
            delete data.virtualData;
        break;
    }
    type = right.type;
    switch(type)
    {
    case CHAR_T:
        data.c = right.data.c;
        break;
    case UCHAR_T:
        data.uc = right.data.uc;
        break;
    case SHORT_T:
        data.s = right.data.s;
        break;
    case USHORT_T:
        data.us = right.data.us;
        break;
    case INT_T:
        data.i = right.data.i;
        break;
    case UINT_T:
        data.ui = right.data.ui;
        break;
    case FLOAT_T:
        data.f = right.data.f;
        break;
    case ASCII_T:
        if(right.data.ascii)
        {
            int len = strlen(right.data.ascii);
            data.ascii = new char[len + 1];
            memset(data.ascii, 0, len + 1);
            strncpy(data.ascii, right.data.ascii, len);
        }
        break;
    case UTF8_T:
        if(right.data.utf8)
        {
            int len = strlen(right.data.utf8);
            data.utf8 = new char[len + 1];
            memset(data.utf8, 0, len + 1);
            strncpy(data.utf8, right.data.utf8, len);
        }
        break;
    case UNICODE_T:
        if(right.data.unicode)
        {
            size_t len = wcslen(right.data.unicode);
            data.unicode = new wchar_t[len + 1];
            memset(data.unicode, 0, sizeof(wchar_t) * (len + 1));
            wcscpy(data.unicode, right.data.unicode);
        }
        break;
    case VECTOR3F_T:
        data.vec3f = new SE_Vector3f;
        *data.vec3f = *right.data.vec3f;
        break;
    case VECTOR4F_T:
        data.vec4f = new SE_Vector4f;
        *data.vec4f = *right.data.vec4f;
        break;
    case VECTOR2F_T:
        data.vec2f = new SE_Vector2f;
        *data.vec2f = *right.data.vec2f;
        break;
    case VECTOR3I_T:
        data.vec3i = new SE_Vector3i;
        *data.vec3i = *right.data.vec3i;
        break;
    case QUAT_T:
        data.quat = new SE_Quat;
        *data.quat = *right.data.quat;
        break;
    case MATRIX3F_T:
        data.mat3f = new SE_Matrix3f;
        *data.mat3f = *right.data.mat3f;
        break;
    case MATRIX2F_T:
        data.mat2f = new SE_Matrix2f;
        *data.mat2f = *right.data.mat2f;
        break;
    case MATRIX4F_T:
        data.mat4f = new SE_Matrix4f;
        *data.mat4f = *right.data.mat4f;
        break;
    case VIRTUALDATA_T:
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
    case CHAR_T:
        ret = data.c == right.data.c;
        break;
    case UCHAR_T:
        ret = data.uc == right.data.uc;
        break;
    case SHORT_T:
        ret = data.s == right.data.s;
        break;
    case USHORT_T:
        ret = data.us == right.data.us;
        break;
    case INT_T:
        ret = data.i == right.data.i;
        break;
    case UINT_T:
        ret = data.ui == right.data.ui;
        break;
    case FLOAT_T:
        ret = data.f == right.data.f;
        break;
    case ASCII_T:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case UTF8_T:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case UNICODE_T:
        ret = !strcmp(data.ascii, right.data.ascii);
        break;
    case VECTOR3F_T:
        ret = *data.vec3f == *right.data.vec3f;
        break;
    case VECTOR4F_T:
        ret = *data.vec4f == *right.data.vec4f;
        break;
    case VECTOR2F_T:
        ret = *data.vec2f == *right.data.vec2f;
        break;
    case VECTOR3I_T:
        ret = *data.vec3i == *right.data.vec3i;
        break;
    case QUAT_T:
        ret = *data.quat == *right.data.quat;
        break;
    case MATRIX3F_T:
        ret = *data.mat3f == *right.data.mat3f;
        break;
    case MATRIX2F_T:
        ret = *data.mat2f == *right.data.mat2f;
        break;
    case MATRIX4F_T:
        ret = *data.mat4f == *right.data.mat4f;
        break;
    case VIRTUALDATA_T:
        ret = data.virtualData->eq(*right.data.virtualData);
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
    type = ASCII_T;
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
