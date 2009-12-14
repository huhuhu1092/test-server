#include "SCommandEventFactoryImpl.h"
#include "SCommandEventDefine.h"
SCommandEvent* SCommandEventFactoryImpl::create(int commandId, unsigned char* data)
{
    switch(commandId)
    {
    case LOGIN:
        {
            SLoginCommandEvent* se = new SLoginCommandEvent;
            se->unpack((char*)data);
            return se;
        }
    }
}
