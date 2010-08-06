#ifndef SE_FILE_H
#define SE_FILE_H
class SE_BufferOutput;
class SE_File
{
public:
    SE_File(const char* name);
    ~SE_File();
    void write(SE_BufferOutput& output);
private:
    FILE* mFile;
};
#endif
