TEMPLATE = lib
TARGET = serverlib
DESTDIR = ../build/serverlib
CONFIG = debug thread console
OBJECTS_DIR = ../build/serverlib
INCLUDEPATH += ../core
unix:LIBS += -lcore -L../build/core
#-static ../build/core/libcore.a
HEADERS += SCommandEventDefine.h \  
           SCommandEventFactoryImpl.h 
SOURCES += SCommandEventFactoryImpl.cpp  \
           SCommandEventDefine.cpp
