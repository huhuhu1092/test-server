#ifndef SE_CHARCODE_H
#define SE_CHARCODE_H
#include <wchar.h>
//char code is unicode in our se engine
class SE_CharCode
{
public:
    SE_CharCode()
    {
        mCode = 0;
    }
    SE_CharCode(wchar_t c)
    {
        mCode = c;
    }
    void set(wchar_t c)
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
    unsigned int toUInt()
    {
        return (unsigned int)mCode;
    }
private:
    wchar_t mCode;
};
#endif
