#ifndef SE_CHARCODE_H
#define SE_CHARCODE_H
//char code is unicode in our se engine
class SE_CharCode
{
public:
    SE_CharCode()
    {
        mCode = 0;
    }
    SE_CharCode(unsigned int c)
    {
        mCode = c;
    }
    void set(unsigned int c)
    {
        mCode = c;
    }
    friend bool operator==(const SE_CharCode& l, const SE_CharCode& r)
    {
        return l.mCode == r.mCode;
    }
    friend bool operator!=(const SE_CharCode& l , const SE_CharCode& r)
    {
        return l.mCode != r.mCode;
    }
    friend bool operator<(const SE_CharCode& l , const SE_CharCode& r)
    {
        return l.mCode < r.mCode;
    }
    friend bool operator>(const SE_CharCode& l , const SE_CharCode& r)
    {
        return l.mCode > r.mCode;
    }
    friend bool operator<=(const SE_CharCode& l , const SE_CharCode& r)
    {
        return l.mCode <= r.mCode;
    }
    friend bool operator>=(const SE_CharCode& l , const SE_CharCode& r)
    {
        return l.mCode >= r.mCode;
    }
    unsigned int toUInt() const
    {
        return (unsigned int)mCode;
    }
private:
    unsigned int mCode;
};
#endif
