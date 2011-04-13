#ifndef SE_PIPE_H
#define SE_PIPE_H
#include "SE_Mutex.h"
enum {SE_LINE_INPUT_MAX_CHAR = 4096};
enum {SE_LINE_OUTPUT_MAX_CHAR = 4096};
struct SE_PipeStruct
{
	int nReadEndInput;
	int nReadEndOutput;
	char szBufferInput[SE_LINE_INPUT_MAX_CHAR];
    SE_Mutex bufferInputMutex;
	char szBufferOutput[SE_LINE_OUTPUT_MAX_CHAR];
    SE_Mutex bufferOutputMutex;
	SE_PipeStruct();
	//Input is for chess engine, chess engine will read data from Input
	//Output is for the shell of chess engine, the shell will read data from Output
	void WriteToInputPipe(const char* content, int size);
	void ReadLineFromInputPipe(char* output);
	void WriteToOutputPipe(const char* content, int size);
	void ReadLineFromOutputPipe(char* output);
};
extern void pipeOpen();
extern bool pipeInputReadLine(char* output);
extern void pipeInputWrite(char* format, ...);
extern bool pipeOutputReadLine(char* output);
extern void pipeOutputWrite(char* format, ...);
#endif
