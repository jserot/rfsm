#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QtCore>
#include <QtGui>
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
    explicit MainWindow(QWidget *parent = 0);

    ~MainWindow();

    void about();

private slots:

    void openFile();
    void newFile();
    void saveFile();
    void saveFileAs();
    void saveAll();
    void closeCurrentFile();
    void closeAll();
    void quit();

    void selectAllText();
    void copyText();
    void cutText();
    void pasteText();

    void textHasBeenModified();
    void outTabChanged(int);

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

    void closeInputFileTab(int index);
    void closeOutputFileTab(int index);

    void zoomIn(); 
    void zoomOut();
    void normalSize();
    void fitToWindow();

private:
    void createViewActions();
    void createMenus();
    void updateViewActions();

    void compile(QString type, QString baseCmd, QString targetDir);
    void dotTransform(QFileInfo f, QString dir);
    void addFileTab(QString fname, QTabWidget *ui_tabs, QList<AppFile*>& files, bool ronly=false, bool isTemp=false);

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

private:

    Ui::MainWindow *ui;

    QFont codeFont;
    QString initDir;

    QList<AppFile*> inFiles;
    QList<AppFile*> outFiles;

    QProcess proc; 

    QAction *zoomInAct;
    QAction *zoomOutAct;
    QAction *normalSizeAct;
    QAction *fitToWindowAct;
};

#endif // MAINWINDOW_H
