#ifndef SE_PROPERTYMANAGER_H
#define SE_PROPERTYMANAGER_H
#include "SE_Common.h"
#include "SE_HashMap.h"
#include "SE_Vector.h"
#include "SE_String.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_PropertyManager_tag
{
    SE_HashMap propertyMap;
} SE_PropertyManager;
extern SE_Result SE_PropertyManager_Init(SE_PropertyManager* propManager, int bucketCount);
extern void SE_PropertyManager_Release(void* pro);
extern float SE_PropertyManager_GetFloat(SE_PropertyManager* propManager, const char* name, float defValue);
extern int SE_PropertyManager_GetInt(SE_PropertyManager* propManager, const char* name, int defValue);
/**
 * copy is indicate whether copy string to out
 * if has no property , out is an empty string
 * */
extern SE_Result SE_PropertyManager_GetString(SE_PropertyManager* propManager, const char* name, const char* defValue, SE_String* out);
extern SE_Result SE_PropertyManager_GetVector3f(SE_PropertyManager* propManager, const char* name, SE_Vector3f* defValue, SE_Vector3f* out);
extern SE_Result SE_PropertyManager_GetVector2f(SE_PropertyManager* propManager, const char* name, SE_Vector2f* defValue, SE_Vector2f* out);
/*
extern SE_Result SE_PropertyManager_GetList(SE_PropertyManager* propManager, const char* name, SE_List* out);
*/
/**   set **/
extern SE_Result SE_PropertyManager_SetFloat(SE_PropertyManager* propManager, const char* name, float v);
extern SE_Result SE_PropertyManager_SetInt(SE_PropertyManager* propManager, const char* name, int v);
extern SE_Result SE_PropertyManager_SetString(SE_PropertyManager* propManager, const char* name, SE_String* v);
extern SE_Result SE_PropertyManager_SetVector3f(SE_PropertyManager* propManager, const char* name, SE_Vector3f* v);
extern SE_Result SE_PropertyManager_SetVector2f(SE_PropertyManager* propManager, const char* name, SE_Vector2f* v);
/*
extern SE_Result SE_PropertyManager_SetList(SE_PropertyManager* propManager, const char* name, SE_List* v);
*/
#ifdef __cplusplus
}
#endif

#endif
