SET(CMAKE_BUILD_TYPE Debug)
SET(ServerLibDir /home/luwei/program/streammedia/streamserver/build/server)
SET(Source
    testSMessageStream.cpp
    )
INCLUDE_DIRECTORIES(../server)
LINK_DIRECTORIES(${SeverLibDir})
ADD_EXECUTABLE(test ${Source})
target_link_libraries(test server)
