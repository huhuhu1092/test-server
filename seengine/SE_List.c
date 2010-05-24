#include "SE_List.h"
#include "SE_Memory.h"
#include <string.h>
static SE_ListNode* createListNode(SE_Element e)
{
    SE_ListNode* ln = (SE_ListNode*)SE_Malloc(sizeof(SE_ListNode));
    if(ln == NULL)
        return NULL;
    SE_Object_Clear(ln, sizeof(SE_ListNode));
    ln->data = e;
    return ln; 
}
static void freeListNode(SE_ListNode* ln)
{
    SE_Element d = ln->data;
    SE_Element_Release(&d);
    SE_Free(ln);
}
SE_Result SE_List_Init(SE_List* list)
{
    SE_ASSERT(list);
    SE_Object_Clear(list, sizeof(SE_List));
    SE_ASSERT(list->head == NULL);
    SE_ASSERT(list->tail == NULL);
    SE_ASSERT(list->size == 0);
    SE_ListNode* ln = (SE_ListNode*)SE_Malloc(sizeof(SE_ListNode));
    if(ln == NULL)
    {
        SE_Object_Clear(list, sizeof(SE_List));
        return SE_INVALID;
    }
    SE_Object_Clear(ln, sizeof(SE_ListNode));
    ln->data.type = SE_NONE;
    list->head = list->tail = ln;
    return SE_VALID;
}
SE_Result SE_List_AddLast(SE_List* list, SE_Element e)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    SE_ListNode* ln = createListNode(e);
    if(ln == NULL)
        return SE_INVALID;
    list->tail->next = ln;
    ln->prev = list->tail;
    list->tail = ln;
    list->size++;
    return SE_VALID;
}
SE_Result SE_List_AddFront(SE_List* list, SE_Element e)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    SE_ListNode* ln = createListNode(e);
    if(ln == NULL)
        return SE_INVALID;
    list->head->next = ln;
    ln->prev = list->head;
    if(list->head == list->tail)
    {
        list->tail = ln;
    }
    list->size++;
    return SE_VALID;
}
SE_Result SE_List_AddBefore(SE_List* list, int index, SE_Element e)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    SE_ASSERT(index >= 0 && index < list->size);
    if(index < 0 || index >= list->size)
        return SE_INVALID;
    SE_ListNode* ln = createListNode(e);
    if(ln == NULL)
        return SE_INVALID;
    int i ;
    SE_ListNode* p = list->head;
    for(i = 0 ; i <= index ; i++)
    {
        p = p->next;
    }
    SE_ASSERT(p != NULL);
    SE_ListNode* prev = p->prev;
    prev->next = ln;
    ln->next = p;
    ln->prev = prev;
    p->prev = ln;
    list->size++;
    return SE_VALID;
}
SE_Result SE_List_RemoveAt(SE_List* list, int index)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    SE_ASSERT(index >= 0 && index < list->size);
    if(index < 0 || index >= list->size)
        return SE_INVALID;
    SE_ListNode* p = list->head;
    int i;
    for(i = 0 ; i <= index ; i++)
    {
        p = p->next;
    }
    SE_ASSERT(p != NULL);
    SE_ListNode* prev = p->prev;
    SE_ListNode* next = p->next;
    SE_ASSERT(prev != NULL);
    if(next == NULL)
    {
        list->tail = prev;
        prev->next = NULL;
    }
    else
    {
        prev->next = next;
        next->prev = prev;
        p->prev = p->next = NULL;
    }
    freeListNode(p);
    list->size--;
    SE_ASSERT(list->size >= 0);
    return SE_VALID;
}
SE_Result SE_List_RemoveFront(SE_List* list)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    if(list->size == 0)
        return SE_INVALID;
    SE_ListNode* p = list->head->next;
    SE_ASSERT(p != NULL);
    if(p == list->tail)
    {
        list->tail = list->head;
        list->head->next = NULL;
    }
    else
    {
        SE_ListNode* next = p->next;
        list->head->next = next;
        next->prev = list->head;
    }
    freeListNode(p);
    list->size--;
    SE_ASSERT(list->size >= 0);
    return SE_VALID;
}
SE_Result SE_List_RemoveLast(SE_List* list)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    if(list->size == 0)
        return SE_INVALID;
    SE_ListNode* p = list->tail;
    list->tail = p->prev;
    list->tail->next = NULL;
    freeListNode(p);
    list->size--;
    SE_ASSERT(list->size >= 0);
    return SE_VALID; 
}
SE_Result SE_List_Apply(SE_List* list, SELISTAPPLYFUNCTION applyFunc, void* context)
{
    SE_ASSERT(list);
    SE_ASSERT(applyFunc);
    SE_ListIterator li;
    SE_Element e;
    SE_Object_Clear(&li, sizeof(SE_ListIterator));
    SE_ListIterator_Init(&li, list);
    while(SE_ListIterator_Next(&li, &e))
    {
        applyFunc(&e, context);
    }
    return SE_VALID;
}
void SE_List_Release(void* l)
{
    SE_ASSERT(l);
    SE_List* list = (SE_List*)l;
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    SE_ListNode* p = list->head;
    while(p != NULL)
    {
        SE_ListNode* next = p->next;
        freeListNode(p);
        p = next;
    }    
    SE_Object_Clear(list, sizeof(SE_List));
}
static void removeListNode(SE_List* l, SE_ListNode* p)
{
    SE_ListNode* head = l->head;
    SE_ListNode* tail = l->tail;
    SE_ListNode* prev = p->prev;
    SE_ListNode* next = p->next;
    prev->next = next;
    if(next != NULL)
    {
        next->prev = prev;
    } 
    else
    {
        SE_ASSERT(p == tail);
        l->tail = prev;
    }
}
SE_Result SE_List_RemoveElement(SE_List* list, SE_Element e)
{
    SE_ASSERT(list);
    SE_ListNode* p = list->head->next;
    if(p == NULL)
        return SE_VALID;
    while(p)
    {
        SE_Element d = p->data;
        if(SE_Element_Compare(d, e) == 0)
            break;
        p = p->next;
    }
    if(p == NULL)
        return SE_VALID;
    removeListNode(list, p);
    SE_Element_Release(&p->data);
    SE_Free(p);
    return SE_VALID;
}

int SE_List_Size(SE_List* list)
{
    SE_ASSERT(list);
    return list->size;
}
SE_Element SE_List_GetAt(SE_List* list , int index)
{
    SE_ASSERT(list);
    SE_ASSERT(list->head != NULL);
    SE_ASSERT(list->tail != NULL);
    if(index < 0 || index >= list->size)
    {
        SE_Element e;
        SE_Object_Clear(&e, sizeof(SE_Element));
        e.type = SE_NONE;
        return e;
    }
    SE_ListNode* p = list->head;
    int i;
    for(i = 0 ; i <= index ; i++)
    {
        p = p->next;
    }
    return p->data;
}
SE_Result SE_ListIterator_Init(SE_ListIterator* li, SE_List* list)
{
    li->list = list;
    li->curr = list->head;
    return SE_VALID;
}
int SE_ListIterator_Next(SE_ListIterator* li, SE_Element* out)
{
    SE_ASSERT(out);
    SE_Object_Clear(out, sizeof(SE_Element));
    SE_ListNode* curr = li->curr;
    if(curr == NULL)
    {
        return 0;
    }
    SE_ListNode* next = curr->next;
    if(next == NULL)
    {
        return 0;
    }
    *out = next->data;
    li->curr = next;
    return 1;
}

