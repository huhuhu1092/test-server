#ifndef SE_STRING_H
#define SE_STRING_H
#include "SE_Common.h"
#ifdef __cplusplus
extern "C" {
#endif
/** this is a C-String, the last char is always 0 */
#define SE_MAX_STR_LEN 4096
typedef struct SE_String_tag
{
    char* data;
    int len;
    int own; //0: not own , 1: own
} SE_String;
/*
 * return the data of SE_String
 * */
extern char* SE_String_GetData(SE_String* str);
/**
 * init from strInput, it assert str is empty before invoke this function
 * */
extern SE_Result SE_String_Init(SE_String* str, const char* strInput);
/**
 * SE_String will get strInput as its data and own be 1
 * when SE_String release it must release data
 * */
extern SE_Result SE_String_Set(SE_String* str, char* strInput);
/**
 * just copy the srcStr's data, len , own to dstStr.
 * dstStr and srcStr just has one can release. this is the thin copy
 * */
extern SE_Result SE_String_SetString(SE_String* dstStr, SE_String* srcStr);
/***
 * after release str->data == NULL and str->len == 0 and str->own == 0
 * */
extern void SE_String_Release(void* strData);
/**
 * if str has data it will first release it, after that it copy strInput
 * */
extern SE_Result SE_String_CopyCharArray(SE_String* str, const char* strInput);
/**
 * the same as before but just in input.
 * */
extern SE_Result SE_String_Copy(SE_String* str, const SE_String* input);
/**
 * copy the sub string from input
 * */
extern SE_Result SE_String_CopyFromSubStr(SE_String* str, const SE_String* input, int start, int len);
/**
 * the length is the length of string which exclude the last '\0'.
 *  */
extern int SE_String_Length(const SE_String* str);
//if data == NULL return true;
//else return false
extern int SE_String_IsEmpty(const SE_String* str);
/**
 * 0: equal
 * 1: >
 * -1: <
 * */
extern int SE_String_Compare(SE_String s1, SE_String s2);
/**
 * return the index of the sub string in src string if found
 * else return -1
 * */
extern int SE_String_FindString(const SE_String* src, const SE_String* sub);
/**
 * concate the element to string
 * */
extern SE_Result SE_String_Concate(SE_String* strDst, const char* fmt, ...);
#ifdef __cplusplus
}
#endif
#endif
