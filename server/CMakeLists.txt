SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../build/core)
SET(Source
    SMainServer.cpp
    SCommandEventFactoryImpl.h
    SCommandEventFactoryImpl.cpp
    SCommandEventDefine.h
    SCommandEventDefine.cpp
    )
INCLUDE_DIRECTORIES(../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(server ${Source})
target_link_libraries(server core)
