#ifndef SWORLD_H
#define SWORLD_H
#include "SObject.h"
#include "STime.h"
#include "SType.h"
class SWorld : public SObject
{
public:
    SUserManager& getUserManager();
    SSimObjectManager& getSimObjectManager();
    SHostManager& getHostManager();
    SSceneManager& getAreaManager();
    void update(STimeMS currTime);
private:
    SUserManager mClientManager;
    SSimObjectManager mSimObjectManager;
    SHostManager mHostManager;
    SSceneManager mAreaManager;
};
#endif
