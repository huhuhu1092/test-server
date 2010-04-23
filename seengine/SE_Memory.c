#include "SE_Memory.h"
#include <stdlib.h>
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

