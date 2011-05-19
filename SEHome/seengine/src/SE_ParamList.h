#ifndef SE_PARAMLIST_H
#define SE_PARAMLIST_H
#include "SE_Value.h"
#include <list>
#include <vector>
#include <algorithm>
class SE_ParamList
{
public:
    void addParam(const SE_Value& v)
    {
        mParams.push_back(v);
    }
    int size() const
    {
        return mParams.size();
    }
    SE_Value getValue(int i) const;
private:
    std::list<SE_Value> mParams;
};
#endif
