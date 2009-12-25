TARGET = createmessagedefine
TEMPLATE = app 
CONFIG += qt console debug
QT -= gui
QT += xml
SOURCES = CreateMessageDefine.cpp \
          main.cpp
HEADERS = CreateMessageDefine.h
DESTDIR = ../build/tools
OBJECTS_DIR = ../build/tools
