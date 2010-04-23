#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Common.h"
#include "SE_String.h"
#ifdef __cplusplus
extern "C" {
#endif
/*
 * this type is used in data structure. for example SE_List, SE_Hash, SE_Tree, etc.
 * **/
enum SE_ELEMENT_TYPE {SE_NONE, SE_INT, SE_UINT, SE_SHORT, SE_USHORT, SE_LONG, SE_ULONG, SE_CHAR, SE_UCHAR, SE_FLOAT, SE_STRING, SE_DATA};
/*
#define SE_NONE   0
#define SE_INT    1
#define SE_UINT   2 
#define SE_SHORT  3 
#define SE_USHORT 4 
#define SE_LONG   5 
#define SE_ULONG  6 
#define SE_FLOAT  7
#define SE_CHAR   8 
#define SE_UCHAR  9
#define SE_STRING 10
#define SE_DATA   11
*/
/*
 * SE_DataPointer
 * */
typedef void (*SE_RELEASE_DATAPOINTER)(void* data);
typedef int (*SE_COMPARE_DATAPOINTER)(void* v1, void* v2);
typedef struct SE_DataPointer_tag
{
    void* data;
    SE_RELEASE_DATAPOINTER fRelease;
    SE_COMPARE_DATAPOINTER fCompare;
} SE_DataPointer;
extern SE_Result SE_DataPointer_Init(SE_DataPointer* dp, SE_RELEASE_DATAPOINTER fRelease, SE_COMPARE_DATAPOINTER fCompare, void* data);
extern void SE_DataPointer_Release(void* dp);
/***
 *  SE_Element
 * */
typedef struct SE_Element_tag
{
    int type; /**SE_ELEMENT_TYPE*/
    union
    {
        char c;
        unsigned char uc;
        short s;
        unsigned short us;
        int i;
        unsigned int ui;
        long l;
        unsigned long ul;
        float f;
        SE_String str;
        SE_DataPointer dp;
    };
} SE_Element;
/*
 * 0: equal
 * 1: >
 * -1: <
 * ***/
extern int SE_Element_Compare(SE_Element v1, SE_Element v2);
extern void SE_Element_Release(void* element);
#ifdef __cplusplus
}

#endif
#endif
