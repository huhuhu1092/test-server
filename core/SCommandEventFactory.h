#ifndef SCOMMANDFACTORY_H
#define SCOMMANDFACTORY_H
class SCommandEvent;
class SCommandEventFactory
{
public:
    SCommandEventFactory(){}
    virtual ~SCommandEventFactory() {};
    virtual SCommandEvent* create(int commandId, unsigned char* data);
};
#endif
