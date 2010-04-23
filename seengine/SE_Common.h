#ifndef SE_COMMON_H
#define SE_COMMON_H
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#ifdef __cplusplus
extern "C" {
#endif

#define SE_ASSERT(x) assert((x))
typedef int SE_Result;
#define SE_VALID 1
#define SE_INVALID 0

/**
 * 0 : invalid
 * 1 : valid
 * */
#define SE_Result_IsValid(r) ((r) != 0)
#define SE_Object_Clear(optr, size) (memset((optr), 0, size))
/*
extern int SE_Result_Valid(SE_Result r);
*/
#ifdef __cplusplus
}
#endif
#endif
