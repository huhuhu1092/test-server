SET(CMAKE_BUILD_TYPE Debug)
SET(Sources 
    SMessageStream.cpp
    SMessageStream.h 
    SUtil.cpp
    SUtil.h
    SMutex.cpp
    SMutex.h
    SSocket.cpp
    SSocket.h
    SNetAddress.cpp
    SNetAddress.h
    SCommandEventFactory.cpp
    SCommandEventFactory.h
    SClientManager.cpp
    SClientManager.h
    SClient.h
    SClient.cpp
    SActivityThread.h
    SActivityThread.cpp
    SObject.h
    SObject.cpp
    SType.h
    SEvent.h
    )
ADD_LIBRARY(core STATIC ${Sources})
