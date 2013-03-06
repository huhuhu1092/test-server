#include "SE_String.h"
#include "SE_Common.h"
#include "SE_Memory.h"
#include <string.h>
#include <stdarg.h>
SE_Result SE_String_Init(SE_String* str, const char* strInput)
{
	size_t len = 0;
    SE_ASSERT(str);
    SE_Object_Clear(str, sizeof(SE_String));
    if(strInput == NULL)
        return SE_INVALID;
    len = strlen(strInput) + 1;
    if(len > SE_MAX_STR_LEN)
        len = SE_MAX_STR_LEN;
    SE_ASSERT(len <= SE_MAX_STR_LEN);
    str->data = (char*)SE_Malloc(len);
    if(str->data == NULL)
        return SE_INVALID;
    str->len = len;
    strncpy(str->data, strInput, len - 1);
    str->data[len - 1] = '\0';
    str->own = 1;
    return SE_VALID;
}
void SE_String_Release(void* strData)
{
	SE_String* str;
    SE_ASSERT(strData);
    str = (SE_String*)strData;
    if(str->own)
        SE_Free(str->data);
    str->data = NULL;
    str->len = 0;
    str->own = 0;
}
SE_Result SE_String_CopyCharArray(SE_String* str, const char* strInput)
{
	int len;
	char* tmpBuffer;
    SE_ASSERT(str);
    if(strInput == NULL)
    {
        if(str->own && str->data)
        {
            SE_Free(str->data);
        }
        str->data = NULL;
        str->len = 0;
        str->own = 0;
        return SE_VALID;
    }
    len = strlen(strInput) + 1;
    if(len > SE_MAX_STR_LEN)
        len = SE_MAX_STR_LEN;
    SE_ASSERT(len >= 1);
    tmpBuffer = (char*)SE_Malloc(len);
    if(tmpBuffer == NULL)
        return SE_INVALID;
    strncpy(tmpBuffer, strInput, len - 1);
    tmpBuffer[len - 1] = '\0';
    if(str->data)
        SE_Free(str->data);
    str->data = tmpBuffer;
    str->len = len;
    str->own = 1;
    return SE_VALID;
}
SE_Result SE_String_Copy(SE_String* str, const SE_String* input)
{
    SE_ASSERT(str);
    if(input == NULL)
        return SE_INVALID;
    return SE_String_CopyCharArray(str, input->data);
}
int SE_String_Length(const SE_String* str)
{
    SE_ASSERT(str);
    if(str->data == NULL)
    {
        SE_ASSERT(str->len == 0);
        return 0;
    }
    else
    {
        SE_ASSERT(str->len > 0 && str->len <= SE_MAX_STR_LEN);
        return str->len - 1;
    }
}
int SE_String_IsEmpty(const SE_String* str)
{
    SE_ASSERT(str);
    if(str->data == NULL)
        return 1;
    else if(strcmp(str->data, "") == 0)
        return 1;
    else
        return 0;
}
SE_Result SE_String_CopyFromSubStr(SE_String* str, const SE_String* input, int start, int len)
{
	int srcLen = 0;
	const char* src = NULL;
	int end, copyLen;
	char* tmpBuffer;
    SE_ASSERT(str);
    if(input == NULL)
        return SE_INVALID;
    srcLen = SE_String_Length(input);
    src = input->data;
    SE_ASSERT(srcLen >= 0 && srcLen < SE_MAX_STR_LEN);
    if(src == NULL)
    {
        SE_ASSERT(srcLen == 0);
        if(str->data && str->own)
        {
            SE_Free(str->data);
        }
        str->data = NULL;
        str->len = 0;
        str->own = 0;
        return SE_VALID;
    } 
    if(start < 0 || start >= srcLen)
        return SE_INVALID;
    if(len == 0)
        return SE_INVALID;
    end = start + len - 1;
    if(end >= srcLen)
        end = srcLen - 1;
    copyLen = end - start + 1;
    tmpBuffer = (char*)SE_Malloc(copyLen + 1);
    if(tmpBuffer == NULL)
        return SE_INVALID;
    strncpy(tmpBuffer, src + start, copyLen);
    tmpBuffer[copyLen] = '\0';
    if(str->data && str->own)
    {
        SE_Free(str->data);
    }
    str->data = tmpBuffer;
    str->len = copyLen + 1;
    str->own = 1;
    return SE_VALID;
}
char* SE_String_GetData(SE_String* str)
{
    return str->data;
}
SE_Result SE_String_Concate(SE_String* strDst, const char* fmt,...)
{
    va_list ap;
    char tmpStr[SE_MAX_STR_LEN];
    va_start(ap, fmt);
    memset(tmpStr, 0, SE_MAX_STR_LEN);
    vsnprintf(tmpStr, SE_MAX_STR_LEN, fmt, ap);
    tmpStr[SE_MAX_STR_LEN - 1] = '\0';
    va_end(ap);
    return SE_String_CopyCharArray(strDst, tmpStr);
}
int SE_String_Compare(SE_String s1, SE_String s2)
{
    if(s1.data == NULL && s2.data == NULL)
        return 0;
    else if(s1.data == NULL && s2.data != NULL)
        return -1;
    else if(s1.data != NULL && s2.data == NULL)
        return 1;
    return strcmp(s1.data, s2.data);
}
SE_Result SE_String_Set(SE_String* str, char* strInput)
{
    SE_ASSERT(str);
    SE_ASSERT(strInput);
    str->data = strInput;
    str->len = strlen(strInput) + 1;
    str->own = 1;
    return SE_VALID;
}
SE_Result SE_String_SetString(SE_String* dstStr, SE_String* srcStr)
{
    dstStr->data = srcStr->data;
    dstStr->len = srcStr->len;
    dstStr->own = srcStr->own;
    return SE_VALID;
}
int SE_String_FindString(const SE_String* src, const SE_String* sub)
{
    const char* p = NULL;
    if(src == NULL || sub == NULL)
        return -1;
    p = strstr(src->data, sub->data);
    if(p == NULL)
        return -1;
    else
        return p - src->data;
}
