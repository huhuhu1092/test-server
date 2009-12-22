#include "SClientConnectionState.h"
SClientConnectedState::SClientConnectedState(SClient* client) : SClientConnectionState(client)
{}
SClientConnectedState::~SClientConnectedState()
{}
bool SClientConnectedState::transition()
{
    return false;
}
///////////////////////
SClientExitingState::SClientExitingState(SClient* client) : SClientConnectionState(client)
{}
////////////
SClientExitedState::SClientExitedState(SClient* client) : SClientConnectionState(client)
{}



