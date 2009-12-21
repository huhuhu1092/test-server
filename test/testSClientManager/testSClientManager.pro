TEMPLATE = app
TARGET = testSClientManager
DESTDIR = ../../build/test/testSClientManager
CONFIG = console debug thread
OBJECTS_DIR = ../../build/test/testSClientManager
INCLUDEPATH = ../../core \
              ../../serverlib
unix : LIBS += -lcore -lserverlib -L../../build/core -L../../build/serverlib
#-static ../../build/core/libcore.a ../../build/serverlib/libserverlib.a
SOURCES += testSClientManager.cpp
