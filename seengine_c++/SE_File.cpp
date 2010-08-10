#include "SE_File.h"
#include "SE_Buffer.h"
SE_File::SE_File(const char* name, IO_TYPE type) : mFile(0)
{
    if(name == NULL)
        return;
    FILE* f = 0;
	if(type == WRITE)
	{
	    f = fopen(name, "wb");
		mType = WRITE;
	}
	else
	{
		f = fopen(name, "rb");
		mType = READ;
	}
    if(f == NULL)
        return;
    mFile = f;
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
    if(!mFile || mType == READ)
        return;
    int len = output.getDataLen();
    const char* data = output.getData();
    fwrite(data, 1, len, mFile);
}
