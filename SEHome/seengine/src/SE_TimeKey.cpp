#include "SE_TimeKey.h"
#include "SE_Buffer.h"
SE_TimeKey SE_TimeKey::INVALID = SE_TimeKey(0xFFFFFFFF);
void SE_TimeKey::read(SE_BufferInput& input)
{
	mKey = input.readInt();
}
void SE_TimeKey::write(SE_BufferOutput& output)
{
	output.writeInt(mKey);
}
