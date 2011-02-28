#ifndef SE_CHARSTYLE_H
#define SE_CHARSTYLE_H
#include <string>
class SE_CharStyle
{
public:
    SE_CharStyle()
    {}
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
    int compare(const SE_CodeStyle& l, const SE_CodeStyle& r)
    {
        return l.mStyle.compare(r.mStyle);
    }
private:
    std::string mStyle;
};
#endif
