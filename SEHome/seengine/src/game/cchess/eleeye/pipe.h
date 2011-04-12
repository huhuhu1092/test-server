#ifndef PIPE_H
#define PIPE_H
struct PipeStruct
{
	enum {LINE_INPUT_MAX_CHAR = 1024};
	enum {LINE_OUTPUT_MAX_CHAR = 2046};
	int nReadEndInput;
	int nReadEndOutput;
	char szBufferInput[LINE_INPUT_MAX_CHAR];
	char szBufferOutput[LINE_OUTPUT_MAX_CHAR];
	PipeStruct();
	//Input is for chess engine, chess engine will read data from Input
	//Output is for the shell of chess engine, the shell will read data from Output
	void WriteToInputPipe(const char* content, int size);
	void ReadLineFromInputPipe(char* output);
	void WriteToOutputPipe(const char* content, int size);
	void ReadLineFromOutputPipe(char* output);
};
extern void pipeInputReadLine(char* output);
extern void pipeInputWrite(char* format, ...);
extern void pipeOutputReadLine(char* output);
extern void pipeOutputWrite(char* format, ...);
#endif
