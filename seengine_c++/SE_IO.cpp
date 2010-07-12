#include "SE_IO.h"
#include "SE_Log.h"
static int getFileSize(FILE* fin)
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
    FILE* fin = fopen(name, "rb");
    outData = NULL;
    outLen = 0;
    if(!fin)
        return;
    int fileSize = getFileSize(fin);
    outData = (char*)SE_Malloc(fileSize);
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
        size_t readNum = fread(p, 1, lenLeft, fp);
        lenLeft -= readNum;
        p += readNum;
    }
    fclose(fin);
}

