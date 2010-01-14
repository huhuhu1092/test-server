TEMPLATE = lib 
DESTDIR = ../build/core
CONFIG = debug thread console
QT -= gui
HEADERS +=  SClient.h \                       
            SCommunicationThreadManager.h \ 
            SMutex.h                   \ 
            SEvent.h \
            SSocket.h \
            SUtil.h \
            SClientManager.h \
            SNetAddress.h \
            SActivityThread.h \
            SLog.h \
            SThread.h \
            SWorkingThreadManager.h \
            SCommandEventFactory.h \
            SObject.h \
            SBufferStream.h \
            SCommandEvent.h \
            SMessageStream.h \
            STime.h \
            SResourceThreadManager.h \
            SType.h \
            SSem.h \
            SOutputThreadManager.h \
            SClientConnectionState.h

SOURCES +=  SLog.cpp \ 
            SSocket.cpp  \
            SUtil.cpp \
            SClientManager.cpp \
            SNetAddress.cpp \
            SThread.cpp \
            SWorkingThreadManager.cpp \
            SResourceThreadManager.cpp \
            SCommunicationThreadManager.cpp \
            SClient.cpp \
            SMutex.cpp \
            SBufferStream.cpp \
            SMessageStream.cpp \
            SObject.cpp \
            SCommandEventFactory.cpp \
            SActivityThread.cpp   \
            STime.cpp \
            SSem.cpp \
            SOutputThreadManager.cpp \
            SClientConnectionState.cpp
OBJECTS_DIR = ../build/core
