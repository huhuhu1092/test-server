#include "SE_TimeKey.h"
#include "SE_Buffer.h"
void SE_TimeKey::read(SE_BufferInput& input)
{
	mKey = input.readInt();
}
void SE_TimeKey::write(SE_BufferOutput& output)
{
	output.writeInt(mKey);
}