#include "SE_Element.h"
#include "SE_Memory.h"
/**
 * SE_DataPointer
 * */
SE_Result SE_DataPointer_Init(SE_DataPointer* dp, SE_RELEASE_DATAPOINTER fRelease, SE_COMPARE_DATAPOINTER fCompare, void* data)
{
    SE_Object_Clear(dp, sizeof(SE_DataPointer));
    dp->data = data;
    dp->fRelease = fRelease; 
    dp->fCompare = fCompare;
    return SE_VALID;
}
void SE_DataPointer_Release(void* dp)
{
    SE_DataPointer* datapointer = (SE_DataPointer*)dp;
    if(datapointer->fRelease && datapointer->data)
    {
        (*datapointer->fRelease)(datapointer->data);
    }
    SE_Free(datapointer->data);

}
/**
 * SE_Element
 * */
int SE_Element_Compare(SE_Element v1, SE_Element v2)
{
    SE_ASSERT(v1.type == v2.type);
    if(v1.type == SE_NONE)
        return 0;
    int ret = 0;
    switch(v1.type)
    {
    case SE_INT: 
        if(v1.i == v2.i)
        {
            ret = 0;
        } 
        else if(v1.i > v2.i)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }
        break; 
    case SE_UINT: 
        if(v1.ui == v2.ui)
        {
            ret = 0;
        } 
        else if(v1.ui > v2.ui)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }
        
        break; 
    case SE_SHORT: 
        if(v1.s == v2.s)
        {
            ret = 0;
        } 
        else if(v1.s > v2.s)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }

        break;
    case SE_USHORT: 
        if(v1.us == v2.us)
        {
            ret = 0;
        } 
        else if(v1.us > v2.us)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }
 
        break; 
    case SE_CHAR: 
        if(v1.c == v2.c)
        {
            ret = 0;
        } 
        else if(v1.c > v2.c)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }
 
        break; 
    case SE_UCHAR: 
        if(v1.uc == v2.uc)
        {
            ret = 0;
        } 
        else if(v1.uc > v2.uc)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }

        break; 
    case SE_LONG: 
        if(v1.l == v2.l)
        {
            ret = 0;
        } 
        else if(v1.l > v2.l)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }

        break; 
    case SE_ULONG: 
        if(v1.ul == v2.ul)
        {
            ret = 0;
        } 
        else if(v1.ul > v2.ul)
        {
            ret = 1;
        }
        else
        {
            ret = -1;
        }

        break; 
    case SE_STRING: 
        ret = SE_String_Compare(v1.str, v2.str);
        break; 
    case SE_DATA: 
        if(v1.dp.fCompare)
        {
            ret = (*v1.dp.fCompare)(v1.dp.data, v2.dp.data);
        }
        else
        { 
            if(v1.dp.data == v2.dp.data)
            {
                ret = 0;
            }
            else if(v1.dp.data > v2.dp.data)
            {
                ret = 1;
            }
            else
            {
                ret = -1;
            }
        }
        break;
    }
    return ret;
}
void SE_Element_Release(void* element)
{
    SE_Element* e = (SE_Element*)element;
    if(e->type == SE_STRING)
    {
        SE_String_Release(&e->str);
    }
    else if(e->type == SE_DATA)
    {
        SE_DataPointer dp = e->dp;
        SE_DataPointer_Release(&e->dp);
    }
}

