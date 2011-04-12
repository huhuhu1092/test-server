#include "pipe.h"
#include <string.h>
#if defined(WIN32)
const char* PATH_SEPERATOR = "\\";
#else
const char* PATH_SEPERATOR = "/";
#endif
static void ParsePath(char *szDir, char *szFile, const char *szPath)
{
    char *lpSeperator = NULL;
    strcpy(szDir, szPath);
    lpSeperator = strrchr(szDir, PATH_SEPERATOR);
    if (lpSeperator == NULL)
    {
        szDir[0] = '\0';
        strcpy(szFile, szPath);
    }
    else
    {
        *lpSeperator = '\0';
        strcpy(szFile, lpSeperator + 1);
    }
}
PipeStruct::PipeStruct()
{
    nReadEndInput = 0;
    nReadEndOutput = 0;
    memset(szBufferInput, 0 , LINE_INPUT_MAX_CHAR);
    memset(szBufferOutput, 0, LINE_OUTPUT_MAX_CHAR);
}
static bool writeContentToBuffer(char* buffer, int readStart, int bufferSize, const char* content, int size)
{
    if(!content)
        return false;
    if(readStart + size >= bufferSize)
        return false;
    memcpy(buffer + readStart, content, size);
    return true;
}
void PipeStruct::WriteToInputPipe(const char* content, int size)
{
    bool ret = writeContentToBuffer(szBufferInput, nReadEndInput, LINE_INPUT_MAX_CHAR, content, size);
    if(ret)
    {
        nReadEndInput += size;
        if(nReadEndInput > LINE_INPUT_MAX_CHAR)
            nReadEndInput = LINE_INPUT_MAX_CHAR;
		
    }
}
static int readLineFromBuffer(char* buffer, int bufferSize, char* output)
{
    char* lpFeedEnd = NULL;
    int nFeedEnd = 0;
    if(!output)
        return 0;
    lpFeedEnd = (char*)memchr(buffer, '\n', bufferSize);
    if(lpFeedEnd == NULL)
    {
        output[0] = '\0';
        return 0;
    }
    else
    {
        nFeedEnd = lpFeedEnd - buffer;
        memcpy(output, buffer, nFeedEnd);
        output[nFeedEnd] = '\0';
        nFeedEnd++;
        bufferSize -= nFeedEnd;
        memcpy(buffer, buffer + nFeedEnd, bufferSize);
        lpFeedEnd = (char*)strchr(output, '\r');
        if(lpFeedEnd)
        {
            *lpFeedEnd = '\0';            
        }
          return nFeedEnd;
    }   
}
void PipeStruct::ReadLineFromInputPipe(char* output)
{
    int size = readLineFromBuffer(szBufferInput, nReadEndInput, output);
    nReadEndInput -= size;
     /*
    char* lpFeedEnd = NULL;
    int nFeedEnd = 0;
    if(!output)
        return;
    lpFeedEnd = (char*)memchr(szBufferInput, '\n', nReadEnd);
    if(lpFeedEnd == NULL)
    {
        output[0] = '\0';
        return;
    }
    else
    {
        nFeedEnd = lpFeedEnd - szBuffer;
        memcpy(output, szBufferInput, nFeedEnd);
        output[nFeedEnd] = '\0';
        nFeedEnd++;
        nReadEnd -= nFeedEnd;
        memcpy(szBufferInput, szBuffer + nFeedEnd, nReadEnd);
        lpFeedEnd = (char*)strchr(output, '\r');
        if(lpFeedEnd)
        {
            *lpFeedEnd = '\0';            
        }
     }
     */
}
void PipeStruct::WriteToOutputPipe(const char* content, int size)
{
    bool ret = writeContentToBuffer(szBufferOutput, nReadEndOutput, LINE_INPUT_MAX_CHAR, content, size);
    if(ret)
    {
        nReadEndOutput += size ;
        if(nReadEndOutput > LINE_OUTPUT_MAX_CHAR)
            nReadEndOutput = LINE_OUTPUT_MAX_CHAR;
    }
}
void PipeStruct::ReadLineFromOutputPipe(char* output)
{
     int size = readLineFromBuffer(szBufferOutput, nReadEndOutput, output);
     nReadEndOutput -= size;
}
///////////////////////////////////////
PipeStruct pipeStd;
void pipeInputReadLine(char* output)
{
    if(!output)
        return;
    pipeStd.ReadLineFromInputPipe(output);
}
void pipeInputWrite(char* format, ...)
{
    char buf[PipeStruct::LINE_OUTPUT_MAX_CHAR];
    memset(buf, 0, PipeStruct::LINE_OUTPUT_MAX_CHAR);
    va_list ap;
    va_start(ap, format);
    vsnprintf(buf, PipeStruct::LINE_OUTPUT_MAX_CHAR, format, ap);
    va_end(ap);
    buf[PipeStruct::LINE_OUTPUT_MAX_CHAR - 1] = 0;
    int size = strlen(buf);
    pipeStd.WriteToInputPipe(buf, size);
}
void pipeOutputReadLine(char* output)
{
    if(!output)
        return;
    pipeStd.ReadLineFromOutputPipe(output);
}
void pipeOutputWrite(char* format, ...)
{
    char buf[PipeStruct::LINE_OUTPUT_MAX_CHAR];
    memset(buf, 0, PipeStruct::LINE_OUTPUT_MAX_CHAR);
    va_list ap;
    va_start(ap, format);
    vsnprintf(buf, PipeStruct::LINE_OUTPUT_MAX_CHAR, format, ap);
    va_end(ap);
    buf[PipeStruct::LINE_OUTPUT_MAX_CHAR - 1] = 0;
    int size = strlen(buf);
    pipeStd.WriteToOutputPipe(buf, size);
}
