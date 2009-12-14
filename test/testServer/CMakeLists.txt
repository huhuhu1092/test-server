SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../../build/core)
SET(Source
    testServer.cpp
    )
INCLUDE_DIRECTORIES(../../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(testServer ${Source})
target_link_libraries(testServer core)
