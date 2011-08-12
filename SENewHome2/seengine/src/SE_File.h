#ifndef SE_FILE_H
#define SE_FILE_H
#include <stdio.h>
class SE_BufferOutput;
class SE_File
{
public:
    enum IO_TYPE {READ, WRITE};
    SE_File(const char* name, IO_TYPE);
    ~SE_File();
    void write(SE_BufferOutput& output);
    static bool isExist(const char* filePath);
private:
    FILE* mFile;
    IO_TYPE mType;
};
#endif
