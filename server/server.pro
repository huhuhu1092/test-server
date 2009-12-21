TEMPLATE = app
TARGET = server
DESTDIR = ../build/server
CONFIG = debug thread console
OBJECTS_DIR = ../build/server
INCLUDEPATH += ../core \
               ../serverlib
unix:LIBS += -lcore -lserverlib -L../build/core -L../build/serverlib
#-static ../build/core/libcore.a ../build/serverlib/libserverlib.a 
SOURCES += SMainServer.cpp
