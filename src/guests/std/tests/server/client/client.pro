TEMPLATE = app
TARGET = client
CONFIG += c++17 console
QT += core widgets network

SOURCES += \
    main.cpp \
    fragment.cpp \
    request.cpp \
    response.cpp \
    compiler.cpp

HEADERS += \
    fragment.h \
    request.h \
    response.h \
    compiler.h
