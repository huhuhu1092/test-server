#ifndef SE_MEMORY_H
#define SE_MEMORY_H
#include "SE_Common.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void* SE_Malloc(size_t size);
extern void SE_Free(void* data);
extern void* SE_Calloc(size_t nelem, size_t esize);
extern void SE_Mem_Copy(const void* src, void* dst, int size);
#ifdef __cplusplus
}

#endif
#endif
