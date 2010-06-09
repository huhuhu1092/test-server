#include "SE_PropertyManager.h"
#include "SE_Element.h"
#include "SE_List.h"
#include "SE_Memory.h"
enum _DataType_E {INT, FLOAT, STRING, VECTOR2f, VECTOR3f, LIST};
SE_Result SE_PropertyManager_Init(SE_PropertyManager* propManager, int bucketCount)
{
    SE_Object_Clear(propManager, sizeof(SE_PropertyManager));
    SE_HashMap_Init(bucketCount, NULL, &propManager->propertyMap);
    return SE_VALID;
}
void SE_PropertyManager_Release(void* pro)
{}
static void getProperty(SE_PropertyManager* propManager, const char* name, enum _DataType_E type, void* defValue, void* out)
{
    SE_Element key;
    SE_Element* value = NULL;
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
	value = SE_HashMap_Get(&propManager->propertyMap, key);
    if(value == NULL)
    {
        switch(type)
		{
		case INT:
			*(int*)out = *(int*)defValue;
			break;
		case FLOAT:
			*(float*)out = *(float*)defValue;
			break;
		case STRING:
			{
				SE_String* outStr = (SE_String*)out;
				SE_String* defString = (SE_String*)defValue;
				SE_String_Init(outStr, SE_String_GetData(defString));
			}
			break;
		case VECTOR2f:
			{
				SE_Vector2f* outVec = (SE_Vector2f*)out;
				SE_Vector2f* defString = (SE_Vector2f*)defValue;
				SE_Vec2f_Copy(defString, &outVec);
			}
			break;
		case VECTOR3f:
			{
				SE_Vector3f* outVec = (SE_Vector3f*)out;
				SE_Vector3f* defString = (SE_Vector3f*)defValue;
				SE_Vec3f_Copy(defString, &outVec);
			}
			break;
		case LIST:
			break;
		}
    }
    else
    { 
        switch(type)
		{
		case INT:
			*(int*)out = value->i;
			break;
		case FLOAT:
			*(float*)out = value->f;
			break;
		case STRING:
			{
				SE_String str = value->str;
				SE_String* outStr = (SE_String*)out;
				SE_String_Copy(&str, outStr);
			}
			break;
		case VECTOR2f:
			{
				SE_Vector2f* vec = (SE_Vector2f*)value->dp.data;
			SE_Vector2f* outVec = (SE_Vector2f*)out;
			SE_Vec2f_Copy(vec, outVec);
			}
			break;
		case VECTOR3f:
			{
			SE_Vector3f* vec = (SE_Vector3f*)value->dp.data;
			SE_Vector3f* outVec = (SE_Vector3f*)out;
			SE_Vec3f_Copy(vec, outVec);
			}
			break;
		case LIST:
			{
				SE_List* l = (SE_List*)value->dp.data;
			SE_List* outList = (SE_List*)out;
					SE_List_Copy(l, outList);
			}
			break;
		}        
    }
    SE_Element_Release(&key);
}
float SE_PropertyManager_GetFloat(SE_PropertyManager* propManager, const char* name, float defValue)
{
    float ret;
    getProperty(propManager, name, FLOAT, &defValue, &ret);
    return ret;
}
int SE_PropertyManager_GetInt(SE_PropertyManager* propManager, const char* name, int defValue)
{
    int ret;
    getProperty(propManager, name, INT, &defValue, &ret);
    return ret;
}
SE_Result SE_PropertyManager_GetString(SE_PropertyManager* propManager, const char* name, const char* defValue, SE_String* out)
{
	SE_String defString;
	SE_String_Init(&defString, defValue);
    getProperty(propManager, name, STRING, &defString, out);
	SE_String_Release(&defString);
    return SE_VALID;
}
SE_Result SE_PropertyManager_GetVector3f(SE_PropertyManager* propManager, const char* name, SE_Vector3f* defValue, SE_Vector3f* out)
{
    getProperty(propManager, name, VECTOR3f, defValue, out);
    return SE_VALID;
}
SE_Result SE_PropertyManager_GetVector2f(SE_PropertyManager* propManager, const char* name, SE_Vector2f* defValue, SE_Vector2f* out)
{
    getProperty(propManager, name, VECTOR2f, defValue, out);
    return SE_VALID;
}
/*
SE_Result SE_PropertyManager_GetList(SE_PropertyManager* propManager, const char* name, SE_List* out)
{
    getProperty(propManager, name, LIST, NULL, out);
    return SE_VALID;
}
*/
SE_Result SE_PropertyManager_SetFloat(SE_PropertyManager* propManager, const char* name, float v)
{
    SE_Element key, value;
    SE_Object_Clear(&key,sizeof(SE_Element));
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value.type = SE_FLOAT;
    value.f = v;
	SE_HashMap_Put(&propManager->propertyMap, key, value);
    return SE_VALID; 
}
SE_Result SE_PropertyManager_SetInt(SE_PropertyManager* propManager, const char* name, int v)
{
    SE_Element key, value;
    SE_Object_Clear(&key,sizeof(SE_Element));
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value.type = SE_INT;
    value.i = v;
	SE_HashMap_Put(&propManager->propertyMap, key, value);
    return SE_VALID;
}
SE_Result SE_PropertyManager_SetString(SE_PropertyManager* propManager, const char* name, SE_String* v)
{
    SE_Element key, value;
    SE_Object_Clear(&key,sizeof(SE_Element));
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value.type = SE_STRING;
    SE_String_Init(&value.str, SE_String_GetData(v));
	SE_HashMap_Put(&propManager->propertyMap, key, value);
    return SE_VALID;
}
SE_Result SE_PropertyManager_SetVector3f(SE_PropertyManager* propManager, const char* name, SE_Vector3f* v)
{
    SE_Element key, value;
    SE_Vector3f* vec;
    SE_Object_Clear(&key,sizeof(SE_Element));
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value.type = SE_DATA;
    vec = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f));
    if(vec)
    {
	SE_Vec3f_Copy(v, vec);
	value.dp.data = vec;
	SE_HashMap_Put(&propManager->propertyMap, key, value);
    }
    else
    {
        SE_String_Release(&key);
    }
    return SE_VALID;
}
SE_Result SE_PropertyManager_SetVector2f(SE_PropertyManager* propManager, const char* name, SE_Vector2f* v)
{
    SE_Element key, value;
    SE_Vector2f* vec;
    SE_Object_Clear(&key,sizeof(SE_Element));
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value.type = SE_DATA;
    vec = (SE_Vector2f*)SE_Malloc(sizeof(SE_Vector2f));
    if(vec)
    {
	    SE_Vec2f_Copy(v, vec);
	    value.dp.data = vec;
		SE_HashMap_Put(&propManager->propertyMap, key, value);
    }
    else
    {
        SE_String_Release(&key);
    }    
    return SE_VALID;
}
/*
SE_Result SE_PropertyManager_SetList(SE_PropertyManager* propManager, const char* name, SE_List* v)
{}
*/
