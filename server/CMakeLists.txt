SET(CMAKE_BUILD_TYPE Debug)
SET(Sources 
    SMessageStream.cpp 
    SUtil.cpp
    SMutex.cpp
    SSocket.cpp
    )
ADD_LIBRARY(server ${Sources})
