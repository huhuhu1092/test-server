#ifndef SCOMMANDEVENTFACTORYIMPL_H
#define SCOMMANDEVENTFACTORYIMPL_H
#include "SCommandEventFactory.h"
class SCommandEventFactoryImpl : public SCommandEventFactory
{
public:
    SCommandEvent* create(int commandId, unsigned char* data);
};
#endif
