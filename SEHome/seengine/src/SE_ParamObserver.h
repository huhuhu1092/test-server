#ifndef SE_PARAMOBSERVER_H
#define SE_PARAMOBSERVER_H
#include "SE_ID.h"
class SE_Value;
class SE_ParamObserver
{
public:
    virtual void update(const SE_AddressID& address, const SE_Value& value) = 0;
};
#endif
