#include "SE_FunctionDict.h"
void SE_FunctionDict::addFunction(const char* funcName, INTERFACE_FUNC fun)
{
    mFuncDict[funcName] = fun;
}
void SE_FunctionDict::removeFunction(const char* funcName)
{
    std::map<std::string, INTERFACE_FUNC>::iterator it = mFuncDict.find(funcName);
    if(it != mFuncDict.end())
        mFuncDict.erase(it);
}
INTERFACE_FUNC SE_FunctionDict::find(const char* funcName)
{
    std::map<std::string, INTERFACE_FUNC>::iterator it = mFuncDict.find(funcName);
    if(it != mFuncDict.end())
        return it->second;
    else
        return NULL;
}

