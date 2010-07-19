#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
#include "SE_Object.h"
class SE_SimObject : public SE_Object
{
    DECLARE_OBJECT(SE_SimObject)
public:
    SE_SimObject();
    virtual ~SE_SimObject();

private:
};
#endif
