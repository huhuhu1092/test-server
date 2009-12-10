SET(CMAKE_BUILD_TYPE Debug)
SET(ServerLibDir ../../build/server)
SET(Source
    testServer.cpp
    )
INCLUDE_DIRECTORIES(../../server)
LINK_DIRECTORIES(${SeverLibDir})
ADD_EXECUTABLE(testServer ${Source})
target_link_libraries(testServer server)
