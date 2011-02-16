#ifndef SE_TIMEKEY_H
#define SE_TIMEKEY_H
class SE_BufferInput;
class SE_BufferOutput;
class SE_TimeKey
{
public:
	SE_TimeKey(unsigned int i = 0)
	{
		mKey = i;
	}
	bool operator==(const SE_TimeKey& r) const
	{
		return mKey == r.mKey;
	}
	bool operator!=(const SE_TimeKey& r) const
	{
		return mKey != r.mKey;
	}
	bool operator <(const SE_TimeKey& r) const
	{
		return mKey < r.mKey;
	}
	bool operator >(const SE_TimeKey& r) const
	{
		return mKey > r.mKey;
	}
	bool operator >=(const SE_TimeKey& r) const
	{
		return mKey >= r.mKey;
	}
	bool operator <=(const SE_TimeKey& r) const
	{
		return mKey <= r.mKey;
	}
	SE_TimeKey operator+(const SE_TimeKey& r) const
	{
		return SE_TimeKey(mKey + r.mKey);
	}
	SE_TimeKey& operator+=(const SE_TimeKey& r) 
	{
		mKey += r.mKey;
		return *this;
	}
	SE_TimeKey operator-(const SE_TimeKey& r) const
	{
		return SE_TimeKey(mKey - r.mKey);
	}
	unsigned int toInt()
	{
		return mKey;
	}
    void read(SE_BufferInput& input);
	void write(SE_BufferOutput& output);
private:
	unsigned int mKey;
};
#endif