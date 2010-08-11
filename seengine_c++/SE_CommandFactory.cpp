#include "SE_CommandFactory.h"
#include <stdlib.h>
static int _Compare(const void* left, const void* right)
{
    const SE_CommandEntry* leftEntry = *(const SE_CommandEntry**)left;
    const SE_CommandEntry* rightEntry = *(const SE_CommandEntry**)right;
    if(leftEntry->id == rightEntry->id)
        return 0;
    else if(leftEntry->id < rightEntry->id)
        return -1;
    else
        return 1;
}
SE_CommandFactory::SE_CommandFactory()
{
    mEntryArray = NULL;
    mEntrySize = 0;
}
SE_CommandFactory::SE_CommandFactory(SE_CommandEntry* entryArray[], int size)
{
    set(entryArray, size);

}
void SE_CommandFactory::set(SE_CommandEntry* entryArray[], int size)
{
    if(entryArray == NULL || size == 0)
    {
        mEntryArray = NULL;
        mEntrySize = 0;
        return;
    }
    mEntryArray = new SE_CommandEntry*[size];
    mEntrySize = size;
    for(int i = 0 ; i < size; i++)
    {
        mEntryArray[i] = entryArray[i];
    }
    qsort(mEntryArray, mEntrySize, sizeof(SE_CommandEntry*), &_Compare);
}
SE_Command* SE_CommandFactory::create(SE_Application* app, const SE_CommandID& id) 
{
    if(mEntrySize == 0)
        return NULL;
    SE_CommandEntry* ce;
	ce = new SE_CommandEntry(id, NULL);
    SE_CommandEntry** entry = (SE_CommandEntry**)bsearch(&ce, mEntryArray, mEntrySize, sizeof(SE_CommandEntry*), &_Compare);
	delete ce;
    if(entry)
    {
        return (*entry)->cf->create(app, id);
    }
    else
       return NULL; 
}
SE_CommandFactory::~SE_CommandFactory()
{
    for(int i = 0 ; i < mEntrySize ; i++)
    {
        delete mEntryArray[i];
    }
    if(mEntryArray)
        delete[] mEntryArray;
}
