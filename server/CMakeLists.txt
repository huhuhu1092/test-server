SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../build/core)
SET(Source
    SResourceThreadManager.cpp
    SWorkingThreadManager.cpp
    SCommunicationThreadManager.cpp
    )
INCLUDE_DIRECTORIES(../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(server ${Source})
target_link_libraries(server core)
