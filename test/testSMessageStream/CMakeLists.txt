SET(CMAKE_BUILD_TYPE Debug)
SET(CoreLibDir ../../build/core)
SET(Source
    testSMessageStream.cpp
    )
INCLUDE_DIRECTORIES(../../core)
LINK_DIRECTORIES(${CoreLibDir})
ADD_EXECUTABLE(test ${Source})
target_link_libraries(test core)
