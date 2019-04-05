/**********************************************************************/
/*                                                                    */
/*              This file is part of the RFSM package                 */
/*                                                                    */
/*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  */
/*                                                                    */
/*  This source code is licensed under the license found in the       */
/*  LICENSE file in the root directory of this source tree.           */
/*                                                                    */
/**********************************************************************/

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QtCore>
#include <QtGui>
#include <QFileSystemModel>
#include "file_filter.h"
#include "app_file.h"
#include "command.h"
#include "syntax_highlighter.h"
#include "imageviewer.h"

namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QString rootPath, QWidget *parent = 0);

    ~MainWindow();

private slots:

  //void openFile();
    void openDir();
    void newFile();
    void saveFile();
    void saveFileAs();
    void saveAll();
    void closeCurrentFile();
    void closeAll();
    void quit();
    void about();
    void select(QModelIndex idx);

    void selectAllText();
    void copyText();
    void cutText();
    void pasteText();

    void textHasBeenModified();
    void tabChanged(int);

    void makeDot();
    void makeSim();
    void makeCTask();
    void makeSystemC();
    void makeVHDL();

    void setGeneralOptions();
    void setPaths();
    void setCodeFont();

    void readProcStdout();
    void readProcStderr();

    void closeFileTab(int index);

    void zoomIn(); 
    void zoomOut();
    void normalSize();
    void fitToWindow();

private:
    void setDir(QString path);
    void createMenus();
    void updateViewActions();
    void createViewActions();

    void compile(QString type, QString baseCmd, QString targetDir);
    void dotTransform(QFileInfo f, QString dir);
    void addFileTab(QString fname, bool ronly=false, bool isTemp=false);

    void openGeneratedFiles(QString type, QString path);
    void openOutputFile(QString type,QString filename, QString dir);

    void saveIndexedFile(int ind);
    void closeIndexedFile(int ind);

    void closeEvent(QCloseEvent *event);

    void readInitFile(void);
    void writeOptionFile(void);

    bool alreadyOpened(QString fname);
    void keyPressed(int key);
    bool executeCmd(QString wDir, QString cmd, bool sync=true);
    void customView(QString toolName, QString fname, QString wDir);

    void runAction(QString wDir, QVector<CommandLine>& actions);
    void runPreActions(QString wDir);
    void runPostActions(QString wDir);

    void scaleImage(double factor);

    ImageViewer* selectedImageViewer();

protected:
    static QStringList excludedFiles;
    static QStringList acceptedSuffixes;

private:
    QFileSystemModel model;
    QFont codeFont, logFont;
    QString initDir;
    FileFilter *modelProxy;
    Ui::MainWindow *ui;

    QList<AppFile*> openedFiles;

    QProcess proc; 

    QAction *zoomInAct;
    QAction *zoomOutAct;
    QAction *normalSizeAct;
    QAction *fitToWindowAct;
};

#endif // MAINWINDOW_H
