#include "SE_Memory.h"
#include <stdlib.h>
#include <string.h>
void* SE_Malloc(size_t size)
{
    return malloc(size);
}
void SE_Free(void* data)
{
    free(data);
}
void* SE_Calloc(size_t nelem, size_t esize)
{
    return calloc(nelem, esize);
}

void SE_Mem_Copy(const void* src, void* dst, int size)
{
	memcpy(dst, src, size);
}