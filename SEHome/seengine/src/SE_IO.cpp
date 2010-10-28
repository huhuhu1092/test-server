#include "SE_IO.h"
#include "SE_Log.h"
#include <stdio.h>
#if defined(WIN32)
#include <Shobjidl.h>
#endif
static int getFileSize(FILE* fp)
{
	int		pos;
	int		end;

	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);

	return end;
}
void SE_IO::readFileAll(const char* fileName, char*& outData, int& outLen)
{
    FILE* fin = fopen(fileName, "rb");
    outData = NULL;
    outLen = 0;
    if(!fin)
        return;
    int fileSize = getFileSize(fin);
    outData = new char[fileSize];
    if(!outData)
    {
        LOGE("out of memory when read file\n");
        return;
    }
    outLen = fileSize;
    size_t lenLeft = fileSize;
    char* p = outData;
    while(lenLeft > 0)
    {
        size_t readNum = fread(p, 1, lenLeft, fin);
        lenLeft -= readNum;
        p += readNum;
    }
    fclose(fin);
}

