#include "SCommandEventFactoryImpl.h"
#include "SCommandEventDefine.h"
SCommandEvent* SCommandEventFactoryImpl::create(int commandId, unsigned char* data)
{
    switch(commandId)
    {
    case LOGIN:
        {
            SLoginCommandEvent* se = new SLoginCommandEvent;
            se->unpack((const char*)data);
            return se;
        }
    }
}
