#ifndef SCOMMANDFACTORY_H
#define SCOMMANDFACTORY_H
class SCommand;
class SCommandFactory
{
public:
    SCommandFactory(){}
    virtual ~SCommandFactory() {};
    virtual SCommand* create(int commandId, unsigned char* data);
};
#endif
