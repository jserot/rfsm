QT       += core gui
QT 		 += xml
QT 		 += widgets

CONFIG += c++11

TARGET = rfsm
TEMPLATE = app

SOURCES += main.cpp\
        mainwindow.cpp \
        ui_mainwindow.cpp \
    options.cpp \
    config.cpp \
    syntax_highlighter.cpp \ 
    ctask_syntax_highlighter.cpp \ 
    fsm_syntax_highlighter.cpp \ 
    imageviewer.cpp \ 
    option.cpp \
    file_filter.cpp \
    project.cpp \
    command.cpp

HEADERS  += mainwindow.h \
    ui_mainwindow.h \
    options.h \
    app_file.h \
    config.h \
    syntax_highlighter.h \
    ctask_syntax_highlighter.h \ 
    fsm_syntax_highlighter.h \ 
    imageviewer.h \ 
    file_filter.h \
    project.h \
    command.h

FORMS    += uis/config.ui

RESOURCES += \
    resources.qrc




















