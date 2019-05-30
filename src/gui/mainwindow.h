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
#include "project.h"
#include "options.h"
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
    explicit MainWindow(QString arg, QWidget *parent = 0);

    ~MainWindow();

private slots:

    void newFile();
    void openFile();
    void saveCurrentFile();
    void saveCurrentFileAs();
    void closeAllResFiles();
    void closeAllFiles();
    void quit();
    void about();

    void newProject();
    void openProject();
    void editProject();
    void addFileToProject();
    void addCurrentFileToProject();
    void saveProject();
    void saveProjectAs();
    void closeProject();

    void makeDotFile();
    void makeDotProject();
    void makeCTaskFile();
    void makeCTaskProject();
    void makeSystemCFile();
    void makeSystemCProject();
    void makeVHDLFile();
    void makeVHDLProject();
    void makeSim();

    void selectAllText();
    void copyText();
    void cutText();
    void pasteText();

    void zoomIn(); 
    void zoomOut();
    void normalSize();
    void fitToWindow();

    void textHasBeenModified();
    void tabChanged0(int index);
    void tabChanged1(int index);

    void setGeneralOptions();
    void setPaths();
    void setCodeFont();

    void readProcStdout();
    void readProcStderr();

    void select(QModelIndex idx);
    void closeFileTab0(int index);
    void closeFileTab1(int index);

private:
    void setDir(QString path);
    void setTreeView(QString path);
    void setupMenus();
    void setupToolbar();
    void updateToolbar(bool, bool);
    void setupFileActions();
    void updateFileActions(bool status);
    void setupProjectActions();
    void updateProjectActions(bool status);
    void setupBuildActions();
    void updateBuildActions(bool, bool);
    void setupEditActions();
    void updateEditActions(bool status);
    void setupViewActions();
    void updateViewActions(ImageViewer *viewer);
    void setupConfigActions();
    void updateActions();

    void openThisFile(QString fname);
    void openProjectFile(QString fname);
    void addFileToProject(QString path);

    void makeDot(bool inProject);
    void makeCTask(bool inProject);
    void makeSystemC(bool inProject);
    void makeVHDL(bool inProject);

    void compile(QString type, QString baseCmd, QString targetDir, bool inProject);
    void dotTransform(QFileInfo f, QString dir);
    void addFileTab(int col, QString fname, bool ronly, bool isTemp);

    void openGeneratedFiles(QString type, QString path);
    void openOutputFile(QString type,QString filename, QString dir);

    void closeIndexedFile(int col, int tabIndex);
    void saveIndexedFile(int col, int tabIndex, QString path);
    void closeCurrentFile(int col);


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

    QString getOptions(QString category, QStringList exclude=QStringList());
    QString getOption(QString name);

    ImageViewer* selectedImageViewer();

protected:
    static QStringList excludedFiles;
    static QStringList acceptedSuffixes;
    static QStringList editableSuffixes;
    static QStringList ignoredAnswerPrefixes;
    static QStringList specialOptions;

    bool ignore_answer(QString r);

    void enableCompileActions(bool status);

private:
    Ui::MainWindow *ui;

    QFileSystemModel model;
    QFont codeFont, logFont;
    QString initDir;
    FileFilter *modelProxy;
    QMap<QWidget*,AppFile*> openedFiles;
    Project* project;
    Options* options;

    QWidget* indexedWidget(int col, int tabIndex);
    AppFile* indexedFile(int col, int tabIndex);

    QProcess proc; 
};

#endif // MAINWINDOW_H
