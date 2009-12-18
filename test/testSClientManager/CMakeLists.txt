SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../../build/core)
SET(Source
    testSClientManager.cpp
    ../../server/SCommandEventDefine.h
    ../../server/SCommandEventDefine.cpp
    )
INCLUDE_DIRECTORIES(../../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(testSClientManager ${Source})
target_link_libraries(testSClientManager core)
