SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../build/core)
SET(Source
    SResourceThreadManager.h
    SResourceThreadManager.cpp
    SWorkingThreadManager.h
    SWorkingThreadManager.cpp
    SCommunicationThreadManager.h
    SCommunicationThreadManager.cpp
    SMainServer.cpp
    )
INCLUDE_DIRECTORIES(../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(server ${Source})
target_link_libraries(server core)
