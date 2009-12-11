SET(CMAKE_BUILD_TYPE Debug)
SET(Sources 
    SMessageStream.cpp 
    SUtil.cpp
    SMutex.cpp
    SSocket.cpp
    SNetAddress.cpp
    SCommandFactory.cpp
    SClientManager.cpp
    SClient.cpp
    SActivityThread.cpp
    SObject.cpp
    )
ADD_LIBRARY(core ${Sources})
