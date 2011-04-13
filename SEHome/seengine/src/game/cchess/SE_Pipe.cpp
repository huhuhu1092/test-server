#include "SE_Pipe.h"
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include "./base/pipe.h"
#if defined(WIN32)
const int PATH_SEPERATOR = '\\';
#else
const int PATH_SEPERATOR = '/';
#endif
//static PipeStruct pipeStd;
SE_PipeStruct::SE_PipeStruct()
{
    nReadEndInput = 0;
    nReadEndOutput = 0;
    memset(szBufferInput, 0 , SE_LINE_INPUT_MAX_CHAR);
    memset(szBufferOutput, 0, SE_LINE_OUTPUT_MAX_CHAR);
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
void SE_PipeStruct::WriteToInputPipe(const char* content, int size)
{
    bufferInputMutex.lock();
    bool ret = writeContentToBuffer(szBufferInput, nReadEndInput, SE_LINE_INPUT_MAX_CHAR, content, size);
    if(ret)
    {
        nReadEndInput += size;
        if(nReadEndInput > SE_LINE_INPUT_MAX_CHAR)
            nReadEndInput = SE_LINE_INPUT_MAX_CHAR;
		
    }
    bufferInputMutex.unlock();
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
void SE_PipeStruct::ReadLineFromInputPipe(char* output)
{
    bufferInputMutex.lock();
    int size = readLineFromBuffer(szBufferInput, nReadEndInput, output);
    nReadEndInput -= size;
    bufferInputMutex.unlock();
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
void SE_PipeStruct::WriteToOutputPipe(const char* content, int size)
{
    bufferOutputMutex.lock();
    bool ret = writeContentToBuffer(szBufferOutput, nReadEndOutput, SE_LINE_INPUT_MAX_CHAR, content, size);
    if(ret)
    {
        nReadEndOutput += size ;
        if(nReadEndOutput > SE_LINE_OUTPUT_MAX_CHAR)
            nReadEndOutput = SE_LINE_OUTPUT_MAX_CHAR;
    }
    bufferOutputMutex.unlock();
}
void SE_PipeStruct::ReadLineFromOutputPipe(char* output)
{
    bufferOutputMutex.lock();
    int size = readLineFromBuffer(szBufferOutput, nReadEndOutput, output);
    nReadEndOutput -= size;
    bufferOutputMutex.unlock();
}
///////////////////////////////////////
SE_PipeStruct sepipeStd;
bool pipeInputReadLine(char* output)
{
    if(!output)
        return false;
    sepipeStd.ReadLineFromInputPipe(output);
	if(output[0] == '\0')
		return false;
	else
		return true;
}
void pipeInputWrite(char* format, ...)
{
    char buf[SE_LINE_OUTPUT_MAX_CHAR];
    memset(buf, 0, SE_LINE_OUTPUT_MAX_CHAR);
    va_list ap;
    va_start(ap, format);
    vsnprintf(buf, SE_LINE_OUTPUT_MAX_CHAR, format, ap);
    va_end(ap);
    buf[SE_LINE_OUTPUT_MAX_CHAR - 1] = 0;
    int size = strlen(buf);
    sepipeStd.WriteToInputPipe(buf, size);
}
bool pipeOutputReadLine(char* output)
{
    if(!output)
        return false;
    sepipeStd.ReadLineFromOutputPipe(output);
	if(output[0] == '\0')
		return false;
	else
		return true;
}
void pipeOutputWrite(char* format, ...)
{
    char buf[SE_LINE_OUTPUT_MAX_CHAR];
    memset(buf, 0, SE_LINE_OUTPUT_MAX_CHAR);
    va_list ap;
    va_start(ap, format);
    vsnprintf(buf, SE_LINE_OUTPUT_MAX_CHAR, format, ap);
    va_end(ap);
    buf[SE_LINE_OUTPUT_MAX_CHAR - 1] = 0;
    int size = strlen(buf);
    sepipeStd.WriteToOutputPipe(buf, size);
}
void pipeOpen()
{
	//pipeStd.Open();
}
