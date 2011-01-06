#ifndef SE_PARAMOBSERVER_H
#define SE_PARAMOBSERVER_H
class SE_Value;
class SE_StringID;
class SE_ParamObserver
{
public:
    virtual void update(const SE_StringID& address, const SE_Value& dataItem) = 0;
};
#endif
