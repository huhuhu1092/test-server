TEMPLATE = app
TARGET = testSMessageStream
CONFIG = console debug thread
DESTDIR = ../../build/test/testSMessageStream
OBJECTS_DIR = ../../build/test/testSMessageStream
INCLUDEPATH = ../../core \
              ../../serverlib
unix : LIBS += -lcore -lserverlib -L../../build/core -L../../build/serverlib
SOURCES += testSMessageStream.cpp
