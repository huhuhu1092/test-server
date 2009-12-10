#ifndef SCOMMAND_H
#define SCOMMAND_H
class SClientManager;
class SCommand
{
public:
    virtual bool handle() = 0;
};
#endif
