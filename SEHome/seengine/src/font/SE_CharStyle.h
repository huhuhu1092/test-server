#ifndef SE_CHARSTYLE_H
#define SE_CHARSTYLE_H
#include <string>
class SE_CharStyle
{
public:
    SE_CharStyle()
    {}
	SE_CharStyle(const std::string s)
	{
		mStyle = s;
	}
    void set(const std::string& str)
    {
        mStyle = str;
    }
    friend bool operator==(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return compare(l, r) == 0;
    }
    friend bool operator!=(const SE_CharStyle& l , const SE_CharStyle& r)
    {
        return compare(l, r) != 0;
    }
    friend bool operator<(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return compare(l, r) < 0;
    }
    friend bool operator>(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return compare(l, r) > 0;
    }
    friend bool operator<=(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return compare(l, r) == 0 || compare(l, r) < 0;
    }
    friend bool operator>=(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return compare(l, r) == 0 || compare(l, r) > 0;
    }
private:
    friend int compare(const SE_CharStyle& l, const SE_CharStyle& r)
    {
        return l.mStyle.compare(r.mStyle);
    }
private:
    std::string mStyle;
};
#endif
