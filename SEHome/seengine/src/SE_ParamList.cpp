#include "SE_ParamList.h"
#include "SE_Utils.h"
SE_Value SE_ParamList::getValue(int i) const
{
    std::list<SE_Value>::const_iterator it = listElementRef(mParams, i);
    if(it != mParams.end())
        return *it;
    else
        return SE_Value();
}
