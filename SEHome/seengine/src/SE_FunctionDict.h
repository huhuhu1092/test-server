#ifndef SE_FUNCTIONDICT_H
#define SE_FUNCTIONDICT_H
#include "SE_ParamList.h"
typedef void (*INTERFACE_FUNC)(SE_ParamList& paramList);
class SE_FunctionDict
{
public:
    void addFunction(const char* funcName, INTERFACE_FUNC fun);
    void removeFunction(const char* funcName);
    INTERFACE_FUNC find(const char* funcName);
private:
    std::map<std::string, INTERFACE_FUNC> mFuncDict;
};
#endif
