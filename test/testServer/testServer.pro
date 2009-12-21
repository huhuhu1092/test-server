TEMPLATE = app
TARGET = testServer
DESTDIR = ../../build/test/testServer
CONFIG += console debug thread
OBJECTS_DIR = ../../build/test/testServer
INCLUDEPATH = ../../core \
              ../../serverlib
unix : LIBS += -lcore -lserverlib -L../../build/core -L../../build/serverlib
SOURCES += testServer.cpp
