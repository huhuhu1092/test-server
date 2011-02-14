#ifndef SE_KEYFRAMECONTROLLER_H
#define SE_KEYFRAMECONTROLLER_H
class SE_BufferInput;
class SE_BufferOutput;
class SE_KeyFrameController
{
public:
	void read(SE_BufferInput& input);
	void write(SE_BufferOutput& output);
};
#endif
