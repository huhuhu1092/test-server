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
    )
ADD_LIBRARY(server ${Sources})
