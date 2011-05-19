#ifndef SE_KEYFRAMECONTROLLER_H
#define SE_KEYFRAMECONTROLLER_H
class SE_BufferInput;
class SE_BufferOutput;
class SE_KeyFrameController
{
public:
    virtual ~SE_KeyFrameController() {}
	void read(SE_BufferInput& input);
	void write(SE_BufferOutput& output);
	virtual SE_KeyFrameController* clone();
};
#endif
