#ifndef SE_UTILS_H
#define SE_UTILS_H

#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Common.h"

#ifdef __cplusplus
extern "C" {
#endif
/** create transform matrix by rotation , scale and translation*/
extern SE_Result SE_CreateTransformByRST(const SE_Quat* rotation, const SE_Vector3f* scale, const SE_Vector3f* translate, SE_Matrix4f* transform);
extern int SE_GetFileSize(FILE* fp);
/** user need to free the data of this function return */
extern void SE_ReadFileAll(FILE* fp, char** outData, int* outLen);
extern void SE_ReadFileAllByName(const char* name, char** outData, int* outLen);
#ifdef __cplusplus
}
#endif /** end __cplusplus*/
#endif /** end SE_UTILS_H*/
