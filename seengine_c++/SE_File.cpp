#include "SE_File.h"
#include <stdio.h>
SE_File::SE_File(const char* name) : mFile(NULL)
{
    if(name == NULL)
        return;
    FILE* fin = fopen(name, "wb");
    if(fin == NULL)
        return;
    mFile = fin;
}
SE_File::~SE_File()
{
    if(mFile)
    {
        fclose(mFile);
    }
}
void SE_File::write(SE_BufferOutput& output)
{
    if(!mFile)
        return;
    int len = output.getDataLen();
    char* data = output.getData();
    fwrite(data, 1, len, mFile);
}
