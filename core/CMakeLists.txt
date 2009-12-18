SET(CMAKE_BUILD_TYPE Debug)
SET(ThreadLibDir /usr/lib)
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
    SCommunicationThreadManager.h
    SCommunicationThreadManager.cpp
    SWorkingThreadManager.h
    SWorkingThreadManager.cpp
    SResourceThreadManager.h
    SResourceThreadManager.cpp
    SThread.h
    SThread.cpp
    SLog.h
    SLog.cpp
    SBufferStream.h
    SBufferStream.cpp
    STime.h
    STime.cpp
    )
INCLUDE_DIRECTORIES(/usr/include)
LINK_DIRECTORIES(${ThreadLibDir})
ADD_LIBRARY(core STATIC ${Sources})
target_link_libraries(core pthread)
