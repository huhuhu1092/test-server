#ifndef SE_TIMEKEY_H
#define SE_TIMEKEY_H
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

private:
	unsigned int mKey;
};
#endif