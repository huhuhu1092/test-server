SET(CMAKE_BUILD_TYPE Debug)
SET(ServerLibDir ../../build/server)
SET(Source
    testSClientManager.cpp
    )
INCLUDE_DIRECTORIES(../../server)
LINK_DIRECTORIES(${SeverLibDir})
ADD_EXECUTABLE(testSClientManager ${Source})
target_link_libraries(testSClientManager server)
