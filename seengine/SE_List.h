#ifndef SE_LIST_H
#define SE_LIST_H

#include "SE_Common.h"
#include "SE_Element.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_ListNode_tag
{
    SE_Element data;
    struct SE_ListNode_tag* prev;
    struct SE_ListNode_tag* next;
} SE_ListNode;
typedef struct SE_List_tag
{
    SE_ListNode* head;
    SE_ListNode* tail;
    int size;
} SE_List;
typedef struct SE_ListIterator_tag
{
    SE_ListNode* curr;
    SE_List* list;
} SE_ListIterator;
extern SE_Result SE_List_Init(SE_List* list);
extern SE_Result SE_List_AddLast(SE_List* list, SE_Element e);
extern SE_Result SE_List_AddFront(SE_List* list, SE_Element e);
extern SE_Result SE_List_AddBefore(SE_List* list, int index, SE_Element e);
extern SE_Result SE_List_RemoveAt(SE_List* list, int index);
extern SE_Result SE_List_RemoveFront(SE_List* list);
extern SE_Result SE_List_RemoveLast(SE_List* list);
extern SE_Result SE_List_RemoveElement(SE_List* list, SE_Element e);
extern void SE_List_Release(void* l);
extern int SE_List_Size(SE_List* list);
extern SE_Element SE_List_GetAt(SE_List* list , int index);
/**/
extern SE_Result SE_ListIterator_Init(SE_ListIterator* li, SE_List* list);
extern int SE_ListIterator_Next(SE_ListIterator* li, SE_Element* out);
#ifdef __cplusplus
}
#endif

#endif
