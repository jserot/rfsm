QT       += core gui
QT 		 += xml
QT 		 += widgets

//CONFIG -= c++11

TARGET = rfsm
TEMPLATE = app

SOURCES += main.cpp\
        mainwindow.cpp \
    options.cpp \
    config.cpp \
    syntax_highlighter.cpp \ 
    ctask_syntax_highlighter.cpp \ 
    fsm_syntax_highlighter.cpp \ 
    imageviewer.cpp \ 
    option.cpp \
    command.cpp

HEADERS  += mainwindow.h \
    options.h \
    app_file.h \
    config.h \
    syntax_highlighter.h \
    ctask_syntax_highlighter.h \ 
    fsm_syntax_highlighter.h \ 
    imageviewer.h \ 
    command.h

FORMS    += uis/mainwindow.ui \
    uis/config.ui

RESOURCES += \
    resources.qrc
































