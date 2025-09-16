TEMPLATE = app
TARGET = client
CONFIG += c++17 console
QT += core widgets network

SOURCES += \
    main.cpp \
    mainWindow.cpp \
    compiler.cpp

HEADERS += \
    mainWindow.h \
    compiler.h

FORMS += \
    mainWindow.ui
