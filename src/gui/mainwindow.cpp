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

#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "options.h"
#include "command.h"
#include "config.h"
#include "fsm_syntax_highlighter.h"
#include "ctask_syntax_highlighter.h"
#include "imageviewer.h"

#include <QDir>
#include <QMap>
#include <QMessageBox>
#include <QFileDialog>
#include <QFontDialog>
#include <QPlainTextEdit>

QStringList MainWindow::excludedFiles = {
      "*~",
      "*.sav", "*.ghw", "*.o", "*.log", "*.output", "*.gif",
      "*_deps.dot", "Makefile"
      };

QStringList MainWindow::acceptedSuffixes = { "fsm", "c", "cpp", "h", "dot", "vcd" };

QStringList MainWindow::editableSuffixes = { "fsm" };

QStringList MainWindow::ignoredAnswerPrefixes = { "** (ImageGlass", "(gtkwave-bin:" };

QStringList MainWindow::specialOptions = { "-dot_external_viewer", "-txt_external_viewer" };

MainWindow::MainWindow(QString projFile, QWidget *parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow),
  modelProxy(NULL),
  project(NULL)
{
  ui->setupUi(this);

  setWindowIcon(QPixmap( ":/img/icon.png" ));
  ui->createMenus(this);
  setupFileActions();
  setupProjectActions();
  setupBuildActions();
  setupEditActions();
  setupViewActions();
  setupConfigActions();
  setupToolbar();
  // ui->inpFilesTab->setMovable(false);

  connect(&proc,SIGNAL(readyReadStandardOutput ()),this,  SLOT(readProcStdout()));
  connect(&proc,SIGNAL(readyReadStandardError ()),this,  SLOT(readProcStderr()));

  connect(ui->filesTab[0], SIGNAL(tabCloseRequested(int)), this, SLOT(closeFileTab0(int)));
  connect(ui->filesTab[0], SIGNAL(currentChanged(int)), this, SLOT(tabChanged0(int)));
  connect(ui->filesTab[1], SIGNAL(tabCloseRequested(int)), this, SLOT(closeFileTab1(int)));
  connect(ui->filesTab[1], SIGNAL(currentChanged(int)), this, SLOT(tabChanged1(int)));

  connect(ui->treeView, SIGNAL(activated(QModelIndex)), this, SLOT(select(QModelIndex)));

  codeFont.setFamily("Courier");
  codeFont.setFixedPitch(true);
  codeFont.setPointSize(11);

  logFont.setPointSize(10);
  ui->logText->setFont(logFont);

  readInitFile();

  if ( ! projFile.isEmpty() ) {
    //QFileInfo f(projFile);
    // openProject(f.canonicalFilePath());
    openProject();
    }

  options = new Options(":options_spec.txt", this);
}

void MainWindow::about(void)
{
  QMessageBox::about(this, tr("RFSM"),  // TO FIX
                     tr("<p>Reactive Finite State Machines</p><p>http://cloud.ip.uca.fr/~serot/rfsm</p><p>(C) 2019, J. Sérot, jocelyn.serot@uca.fr</p>"));
}

MainWindow::~MainWindow()
{
  delete ui;
}

// UI-related fonctions

void MainWindow::setupFileActions()
{
  QObject::connect(ui->actionNewFile, SIGNAL(triggered()), this, SLOT(newFile()));
  QObject::connect(ui->actionOpenFile, SIGNAL(triggered()), this, SLOT(openFile()));
  QObject::connect(ui->actionSaveCurrentFile, SIGNAL(triggered()), this, SLOT(saveCurrentFile()));
  QObject::connect(ui->actionSaveCurrentFileAs, SIGNAL(triggered()), this, SLOT(saveCurrentFileAs()));
  //QObject::connect(ui->actionCloseFile, SIGNAL(triggered()), this, SLOT(closeCurrentFile()));
  QObject::connect(ui->actionCloseResFiles, SIGNAL(triggered()), this, SLOT(closeAllResFiles()));
  QObject::connect(ui->actionCloseAllFiles, SIGNAL(triggered()), this, SLOT(closeAllFiles()));
  QObject::connect(ui->actionQuit, SIGNAL(triggered()), this, SLOT(quit()));
  QObject::connect(ui->actionAbout, SIGNAL(triggered()), this, SLOT(about()));
  updateFileActions(false);
}

void MainWindow::updateFileActions(bool status)
{
  ui->actionSaveCurrentFile->setEnabled(status);
  ui->actionSaveCurrentFileAs->setEnabled(status);
  ui->actionCloseFile->setEnabled(status);
  ui->actionCloseAllFiles->setEnabled(status);
  // Other actions are always enabled
}

void MainWindow::setupProjectActions()
{
  QObject::connect(ui->actionNewProject, SIGNAL(triggered()), this, SLOT(newProject()));
  QObject::connect(ui->actionOpenProject, SIGNAL(triggered()), this, SLOT(openProject()));
  QObject::connect(ui->actionAddCurrentFileToProject, SIGNAL(triggered()), this, SLOT(addCurrentFileToProject()));
  QObject::connect(ui->actionAddFileToProject, SIGNAL(triggered()), this, SLOT(addFileToProject()));
  QObject::connect(ui->actionEditProject, SIGNAL(triggered()), this, SLOT(editProject()));
  QObject::connect(ui->actionSaveProject, SIGNAL(triggered()), this, SLOT(saveProject()));
  QObject::connect(ui->actionSaveProjectAs, SIGNAL(triggered()), this, SLOT(saveProjectAs()));
  QObject::connect(ui->actionCloseProject, SIGNAL(triggered()), this, SLOT(closeProject()));
  updateProjectActions(false);
}

void MainWindow::updateProjectActions(bool status)
{
  ui->actionAddFileToProject->setEnabled(status);
  ui->actionAddCurrentFileToProject->setEnabled(status);
  ui->actionEditProject->setEnabled(status);
  ui->actionCloseProject->setEnabled(status);
  ui->actionSaveProject->setEnabled(status);
  ui->actionSaveProjectAs->setEnabled(status);
  // Other actions are always enabled
}

void MainWindow::setupBuildActions()
{
  QObject::connect(ui->actionBuildDotFile, SIGNAL(triggered()), this, SLOT(makeDotFile()));
  QObject::connect(ui->actionBuildCTaskFile, SIGNAL(triggered()), this, SLOT(makeCTaskFile()));
  QObject::connect(ui->actionBuildSystemCFile, SIGNAL(triggered()), this, SLOT(makeSystemCFile()));
  QObject::connect(ui->actionBuildVHDLFile, SIGNAL(triggered()), this, SLOT(makeVHDLFile()));
  QObject::connect(ui->actionBuildDotProject, SIGNAL(triggered()), this, SLOT(makeDotProject()));
  QObject::connect(ui->actionBuildCTaskProject, SIGNAL(triggered()), this, SLOT(makeCTaskProject()));
  QObject::connect(ui->actionBuildSystemCProject, SIGNAL(triggered()), this, SLOT(makeSystemCProject()));
  QObject::connect(ui->actionBuildVHDLProject, SIGNAL(triggered()), this, SLOT(makeVHDLProject()));
  QObject::connect(ui->actionRunSim, SIGNAL(triggered()), this, SLOT(makeSim()));
  updateBuildActions(false);
}

void MainWindow::updateBuildActions(bool status)
{
  ui->actionBuildDotFile->setEnabled(status);
  ui->actionBuildCTaskFile->setEnabled(status);
  ui->actionBuildSystemCFile->setEnabled(status);
  ui->actionBuildVHDLFile->setEnabled(status);

  ui->actionBuildDotProject->setEnabled(project != NULL && status);
  ui->actionBuildCTaskProject->setEnabled(project != NULL && status);
  ui->actionBuildSystemCProject->setEnabled(project != NULL && status);
  ui->actionBuildVHDLProject->setEnabled(project != NULL && status);
  ui->actionRunSim->setEnabled(project != NULL && status);
}

void MainWindow::setupEditActions()
{
  QObject::connect(ui->actionCopy, SIGNAL(triggered()), this, SLOT(copyText()));
  QObject::connect(ui->actionCut, SIGNAL(triggered()), this, SLOT(cutText()));
  QObject::connect(ui->actionPaste, SIGNAL(triggered()), this, SLOT(pasteText()));
  QObject::connect(ui->actionSelect, SIGNAL(triggered()), this, SLOT(selectAllText()));
}

void MainWindow::setupViewActions()
{
  connect(ui->actionZoomIn, SIGNAL(triggered()), this, SLOT(zoomIn()));
  connect(ui->actionZoomOut, SIGNAL(triggered()), this, SLOT(zoomOut()));
  connect(ui->actionNormalSize, SIGNAL(triggered()), this, SLOT(normalSize()));
  connect(ui->actionFitToWindow, SIGNAL(triggered()), this, SLOT(fitToWindow()));
  updateViewActions(NULL);
}

void MainWindow::updateViewActions(ImageViewer *viewer)
{
  if ( viewer ) {
    bool b = viewer->isFittedToWindow();
    ui->actionFitToWindow->setEnabled(true);
    ui->actionFitToWindow->setChecked(b);
    ui->actionZoomIn->setEnabled(!b);
    ui->actionZoomOut->setEnabled(!b);
    ui->actionNormalSize->setEnabled(!b);
    }
  else {
    ui->actionFitToWindow->setEnabled(false);
    ui->actionZoomIn->setEnabled(false);
    ui->actionZoomOut->setEnabled(false);
    ui->actionNormalSize->setEnabled(false);
    }
}

void MainWindow::setupConfigActions()
{
  QObject::connect(ui->actionPathConfig,SIGNAL(triggered()),this,SLOT(setPaths()));
  QObject::connect(ui->actionGeneralOptions, SIGNAL(triggered()), this, SLOT(setGeneralOptions()));
  QObject::connect(ui->actionFontConfig, SIGNAL(triggered()), this, SLOT(setCodeFont()));
}

void MainWindow::setupToolbar()
{
  connect(ui->openProjectButton, SIGNAL(clicked()), this, SLOT(openProject()));
  connect(ui->openFileButton, SIGNAL(clicked()), this, SLOT(openFile()));
  connect(ui->newFileButton, SIGNAL(clicked()), this, SLOT(newFile()));
  // connect(ui->saveFileButton, SIGNAL(clicked()), this, SLOT(saveCurrentFile()));
  // connect(ui->saveAllButton, SIGNAL(clicked()), this, SLOT(saveAll()));

  connect(ui->compileDotFileButton, SIGNAL(clicked()), this, SLOT(makeDotFile()));
  connect(ui->compileCTaskFileButton, SIGNAL(clicked()), this, SLOT(makeCTaskFile()));
  connect(ui->compileSystemcFileButton, SIGNAL(clicked()), this, SLOT(makeSystemCFile()));
  connect(ui->compileVHDLFileButton, SIGNAL(clicked()), this, SLOT(makeVHDLFile()));

  connect(ui->compileDotProjectButton, SIGNAL(clicked()), this, SLOT(makeDotProject()));
  connect(ui->compileCTaskProjectButton, SIGNAL(clicked()), this, SLOT(makeCTaskProject()));
  connect(ui->compileSystemcProjectButton, SIGNAL(clicked()), this, SLOT(makeSystemCProject()));
  connect(ui->compileVHDLProjectButton, SIGNAL(clicked()), this, SLOT(makeVHDLProject()));
  connect(ui->runSimButton, SIGNAL(clicked()), this, SLOT(makeSim()));

  updateToolbar(false);
}

void MainWindow::updateToolbar(bool status)
{
  ui->compileDotFileButton->setEnabled(status);
  ui->compileCTaskFileButton->setEnabled(status);
  ui->compileSystemcFileButton->setEnabled(status);
  ui->compileVHDLFileButton->setEnabled(status);
  ui->compileDotProjectButton->setEnabled(project != NULL && status);
  ui->compileCTaskProjectButton->setEnabled(project != NULL && status);
  ui->compileSystemcProjectButton->setEnabled(project != NULL && status);
  ui->compileVHDLProjectButton->setEnabled(project != NULL && status);
  ui->runSimButton->setEnabled(project != NULL && status);
}

void MainWindow::updateActions(void)
{
  int currentSrcTab = ui->filesTab[0]->count() > 0 ? ui->filesTab[0]->currentIndex() : -1;
  QString currentFile = currentSrcTab >= 0 ? ui->filesTab[0]->tabText(currentSrcTab) : "";
  bool srcFileSelected = !currentFile.isEmpty() && currentFile.endsWith(".fsm");
  updateFileActions(srcFileSelected);
  updateProjectActions(project != NULL);
  updateBuildActions(project != NULL || srcFileSelected);
  updateToolbar(srcFileSelected);
  // updateEditActions(status); // TODO
  updateViewActions(selectedImageViewer());
}

void MainWindow::setTreeView(QString path)
{
    model.setRootPath(path);
    QModelIndex rootModelIndex = model.setRootPath(path);

    if ( modelProxy ) delete modelProxy;
    modelProxy = new FileFilter(&model, excludedFiles);
    modelProxy->setSourceModel(&model);
    //modelProxy->setSortRole(QFileSystemModel::FileNameRole);
    //qDebug() << "Sort column : " << modelProxy->sortColumn();
    model.fetchMore(model.index(0,0));  // Do not omit !
    modelProxy->sort(3, Qt::DescendingOrder);  // Files first, then directories 
    //qDebug() << "Sort column : " << modelProxy->sortColumn();
    ui->treeView->setModel(modelProxy);
    if (!path.isEmpty()) {
      QModelIndex rootIndex = model.index(QDir::cleanPath(path));
      if (rootIndex.isValid()) ui->treeView->setRootIndex(modelProxy->mapFromSource(rootModelIndex));
      }
    for ( int i=1; i<ui->treeView->header()->count(); i++ ) ui->treeView->hideColumn(i);
    ui->treeView->setHeaderHidden(true);
}

// Reading .ini file

void MainWindow::readInitFile(void)
{
  QString filedir = QApplication::applicationDirPath();
  filedir.append("/rfsm.ini");
  QFile* iniFile = new QFile(filedir);
  if ( ! iniFile->exists() ) setPaths();
  ui->logText->append("Reading " + iniFile->fileName());
  iniFile->open(QIODevice::ReadOnly | QIODevice::Text);
  QTextStream flux(iniFile);
  while( ! flux.atEnd() ) {
      QStringList l = flux.readLine().split("=");
      if ( l.length() == 2 && l[0] != "" && l[1] != "" ) {
          QString key = l[0].trimmed();
          QString val = l[1].trimmed();
          //qDebug() << "  " << key << "=" << val;
          ui->logText->append("  " + key + "=" + val);
          if ( key == "COMPILER" )
            config::getInstance()->setPath("compiler", val);
          else if ( key == "DOTPROGRAM" )
            config::getInstance()->setPath("dotProgram", val);
          else if ( key == "DOTVIEWER" )
            config::getInstance()->setPath("dotViewer", val);
          else if ( key == "VCDVIEWER" )
            config::getInstance()->setPath("vcdViewer", val);
          else if ( key == "TXTVIEWER" )
            config::getInstance()->setPath("txtViewer", val);
          else if ( key == "INITDIR" )
            initDir = val;
        }
    }
  flux.flush();
  iniFile->close();
}

void writeInitfile(void)
{
  QString filedir = QApplication::applicationDirPath();
  filedir.append("/rfsm.ini");
  QFile* fic= new QFile(filedir);
  if ( fic->exists() ) fic->remove();
  if ( fic->open(QIODevice::WriteOnly | QIODevice::Text) == false ) return;
  QTextStream flux(fic);
  flux.setCodec("UTF-8");
  flux << "COMPILER=" << config::getInstance()->getPath("compiler") << endl;
  flux << "DOTPROGRAM=" << config::getInstance()->getPath("dotProgram") << endl;
  flux << "DOTVIEWER=" << config::getInstance()->getPath("dotViewer") << endl;
  flux << "VCDVIEWER=" << config::getInstance()->getPath("vcdViewer") << endl;
  flux << "TXTVIEWER=" << config::getInstance()->getPath("txtViewer") << endl;
  flux.flush();
  fic->close();
}


// TABed file management

QWidget* MainWindow::indexedWidget(int col, int tabIndex)
{
  QWidget *r = ui->filesTab[col]->widget(tabIndex);
  return r;
}

AppFile* MainWindow::indexedFile(int col, int tabIndex)
{
  QWidget* w = indexedWidget(col, tabIndex);
  if ( w == NULL ) return NULL;
  if ( ! openedFiles.contains(w) ) return NULL; 
  return openedFiles.value(w);
}

void MainWindow::closeIndexedFile(int col, int index)
{
  QWidget* w = indexedWidget(col, index);
  if ( w == NULL ) return;
  AppFile* f = indexedFile(col, index);
  if ( f == NULL ) return;
  qDebug() << "Closing file " << f->info.canonicalFilePath(); 
  if ( f->upToDate == false ) {
      QMessageBox msgBox;
      msgBox.setText("File unsaved.");
      msgBox.setInformativeText("Do you want to save your changes ?");
      msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard);
      msgBox.setDefaultButton(QMessageBox::Save);
      switch ( msgBox.exec() ) {
        case QMessageBox::Save:
          //saveOpenedFile(f);
          break;
        case QMessageBox::Discard:
          break;
        default:
          break;
        }
    }
  delete f;
  openedFiles.remove(w);
}

void MainWindow::closeFileTab0(int index)
{
  closeIndexedFile(0,index);
  ui->filesTab[0]->removeTab(index);
}

void MainWindow::closeFileTab1(int index)
{
  closeIndexedFile(1,index);
  ui->filesTab[1]->removeTab(index);
}

void MainWindow::tabChanged0(int index)
{
  Q_UNUSED(index);
  updateActions();
}

void MainWindow::tabChanged1(int index)
{
  Q_UNUSED(index);
  // Nothing - nothing should change in result tabs
}

QString changeSuffix(QString fname, QString suffix)
{
  QFileInfo f(fname);
  return f.path() + "/" + f.completeBaseName() + suffix;
}

SyntaxHighlighter* makeSyntaxHighlighter(QString suffix, QTextDocument* doc)
{
    if ( suffix == "fsm" ) return new FsmSyntaxHighlighter(doc);
    if ( suffix == "c" || suffix == "h" || suffix == "cpp" ) return new CTaskSyntaxHighlighter(doc);
    return NULL;
}

void MainWindow::addFileTab(int col, QString fname, bool ronly, bool isTemp)
{
  QFile file(fname);
  QFileInfo f(fname);
  if ( ! file.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+fname);
      return;
    }
  if ( f.suffix() == "gif" ) {
    QPixmap pixmap(f.filePath());
    ImageViewer *viewer = new ImageViewer();
    viewer->setPixmap(pixmap);
    viewer->setWhatsThis("ImageViewer");
    ui->filesTab[col]->addTab(viewer, changeSuffix(f.fileName(),".dot"));
    //AppFile* openedFile = new AppFile(fname, true, viewer, NULL);
    //openedFiles.insert(edit, openedFile);
    QSize sz1 = pixmap.size();
    QSize sz2 = ui->filesTab[col]->currentWidget()->size();
    viewer->scaleImage((double)sz2.height()/sz1.height());
    } 
  else {
    QPlainTextEdit* edit = new QPlainTextEdit();
#ifdef Q_OS_MACOS
    edit->setFont(codeFont);
#endif
    edit->setWhatsThis("TextEditor");
    edit->setPlainText(QString::fromUtf8(file.readAll()));
    edit->setReadOnly(ronly);
    ui->filesTab[col]->addTab(edit, isTemp ? "new" : f.fileName());
    SyntaxHighlighter* highlighter = makeSyntaxHighlighter(f.suffix(), edit->document());
    AppFile* openedFile = new AppFile(fname, ronly, edit, highlighter);
    openedFiles.insert(edit, openedFile);
    if ( ! ronly ) QObject::connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(textHasBeenModified()));
    }
  ui->filesTab[col]->setCurrentIndex(ui->filesTab[col]->count()-1);
  updateActions();
}


// FILE operations 

void MainWindow::newFile()
{
  QString path = QFileDialog::getSaveFileName(this, "Select location for file", initDir);
  if ( path.isEmpty() ) return;
  if ( ! path.endsWith(".fsm") ) return;
  QFile f(path);
  f.open(QIODevice::ReadWrite);
  qDebug() << "Created file " << path;
  addFileTab(0, path, false, false);
  updateActions();
}

void MainWindow::openFile()
{
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open Source File"),
                                                initDir,
                                                tr("Source files (*.fsm)"));
  if ( fileName.isEmpty() ) return;
  QFileInfo f(fileName);
  ui->logText->append("Opening file " + f.canonicalFilePath());
  addFileTab(0, f.canonicalFilePath(), false, false);
  updateActions();
}

void MainWindow::saveCurrentFile()
{
  if ( ui->filesTab[0]->count() == 0 ) return;
  int ind = ui->filesTab[0]->currentIndex();
  saveIndexedFile(0, ind, ""); // Only source files can be saved
}

void MainWindow::saveCurrentFileAs()
{
  if ( ui->filesTab[0]->count() == 0 ) return;
  QString saveName = QFileDialog::getSaveFileName(this, "Select location to save file", initDir);
  if ( saveName.isEmpty() ) return;
  int ind = ui->filesTab[0]->currentIndex();
  saveIndexedFile(0, ind, saveName); // Only source files can be saved
}

void MainWindow::saveIndexedFile(int col, int ind, QString newSavePath)
{
  AppFile *f = indexedFile(col, ind);
  if ( f == NULL ) return;
  QString path = newSavePath.isEmpty() ? f->info.canonicalFilePath() : newSavePath;
  QPlainTextEdit* text = f->text;
  if ( text == NULL ) return;
  qDebug() << "Saving " << ui->filesTab[col]->tabText(ind) << " as " << path;
  QFile save(path);
  if ( ! save.open(QFile::WriteOnly | QFile::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+ path + " for writing");
      return;
    }
  QTextStream os(&save);
  os << text->toPlainText();
  save.flush();
  save.close();
  if ( newSavePath.isEmpty() ) {
    QString t = ui->filesTab[col]->tabText(ind);
    if ( t.endsWith("*") ) {
      t.replace("*","");
      ui->filesTab[col]->setTabText(ind,t);
      f->upToDate = true;
      }
    }
  else { // The easiest way to handle this is just to close the current tab and reopen it
    closeCurrentFile(col);
    QFileInfo ff(newSavePath);
    addFileTab(col, ff.canonicalFilePath(), false, false);
    }
}

void MainWindow::closeCurrentFile(int col)
{
  switch ( col ) {
  case 0: closeFileTab0(ui->filesTab[0]->currentIndex()); break;
  case 1: closeFileTab1(ui->filesTab[0]->currentIndex()); break;
  default: break;
  }
  updateActions();
}

void MainWindow::closeAllResFiles()
{
  while ( ui->filesTab[1]->count() > 0 )
    closeFileTab1(ui->filesTab[1]->currentIndex());
  updateActions();
}

void MainWindow::closeAllFiles()
{
  while ( ui->filesTab[1]->count() > 0 )
    closeFileTab1(ui->filesTab[1]->currentIndex());
  while ( ui->filesTab[0]->count() > 0 )
    closeFileTab0(ui->filesTab[0]->currentIndex());
  updateActions();
}

// void MainWindow::saveAll()
// {
//   for ( int ind=0; ind<ui->filesTab->count(); ind++ ) {
//       ui->filesTab->setCurrentIndex(ind);
//       saveIndexedFile(ind);
//     }
// }

// PROJECT operations

void MainWindow::newProject()
{
  // QString dirName = QFileDialog::getExistingDirectory(this, tr("New Project Directory"), initDir,
  //                                                 QFileDialog::ShowDirsOnly
  //                                                 | QFileDialog::DontResolveSymlinks);
  QString path = QFileDialog::getSaveFileName(this, "Select location for project file and directory", initDir);
  if ( path.isEmpty() ) return;
  ui->logText->append("Creating project " + path);
  QFile f(path);
  if ( ! f.open(QFile::WriteOnly | QFile::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+ path + " for writing");
      return;
    }
  // TODO: create subdirs ?
  QFileInfo ff(path);
  closeAllFiles();
  project = new Project();
  project->writeToFile(ff.canonicalFilePath());
  setTreeView(ff.canonicalPath());
  updateActions();
}

void MainWindow::openProject()
{
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project File"),
                                                initDir,
                                                tr("Project files (*.pro)"));
  if ( fileName.isEmpty() ) return;
  QFileInfo f(fileName);
  ui->logText->append("Opening project " + f.canonicalFilePath());
  closeAllFiles();
  if ( project != NULL ) closeProject();
  project = new Project(f.canonicalFilePath());
  QString msg;
  if ( ( msg = options->readFromProject(project)) != "" )
    QMessageBox::warning(this, "", "Error reading project file " + f.fileName() + ": " + msg);
  ui->logText->append("Updated compiler options with settings from file " + f.fileName());
  setTreeView(f.canonicalPath());
  updateActions();
}

QStringList getProjFile(QString path)
{
    QDir dir(path);
    return dir.entryList(QStringList () << "*.pro");
}

void MainWindow::saveProject()
{
  if ( project == NULL ) return;
  QString path = project->file;
  ui->logText->append("Saving project file " + path);
  QFile save(path);
  if ( ! save.open(QFile::WriteOnly | QFile::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+ path + " for writing");
      return;
    }
  QTextStream os(&save);
  if ( ! project->mainName.isEmpty() )
    os << "MAIN=" << project->mainName << endl;
  os << "SRCS=" << project->srcFiles.join(" ") << endl;
  // We save the options currently recorded in the configuration !
  os << "DOT_OPTS=" << getOptions("dot") << endl;
  os << "CTASK_OPTS=" << getOptions("ctask") << endl;
  os << "SYSTEMC_OPTS=" << getOptions("systemc") << endl;
  os << "VHDL_OPTS=" << getOptions("vhdl") << endl;
  os << "SIM_OPTS=" << getOptions("sim") << endl;
  save.flush();
  save.close();
}

void MainWindow::saveProjectAs()
{
  QString saveName = QFileDialog::getSaveFileName(this, "Select location to save file", initDir);
  if ( saveName.isEmpty() ) return;
  project->file = saveName;
  saveProject();
  QFileInfo f(saveName);
  project->dir = f.canonicalPath();
}

void MainWindow::editProject()
{
  // TODO: bring up form for setting project name ans source files + compiler options
  // Meanwhile: simply the .pro file as text
  if ( project == NULL ) return;
  addFileTab(0, project->file, false, false);
}

void MainWindow::addCurrentFileToProject()
{
  if ( project == NULL ) return;
  if ( ui->filesTab[0]->count() == 0 ) return;
  int ind = ui->filesTab[0]->currentIndex();
  AppFile *f = indexedFile(0, ind);
  if ( f == NULL ) return;
  addFileToProject(f->info.canonicalFilePath());
}

void MainWindow::addFileToProject()
{
  // Get file from dialog, copy to project dir, add to srcs list 
  if ( project == NULL ) return;
  QString path = QFileDialog::getOpenFileName(this, tr("Select Source File"),
                                                initDir,
                                                tr("Source files (*.fsm)"));
  if ( path.isEmpty() ) return;
  addFileToProject(path);
}

void MainWindow::addFileToProject(QString path)
{
  QFileInfo fi(path);
  QString fname = fi.fileName();
  for ( int i=0; i<project->srcFiles.length(); i++ ) {
    if ( fname == project->srcFiles.at(i) ) {
      QMessageBox::warning(this, "", "File " + fname + " already belongs to project");
      return;
      }
    }
  QFile f(path);
  QString dst = project->dir + "/" + fi.fileName();
  f.copy(dst);
  ui->logText->append("File " + fi.canonicalFilePath() + " copied to " + dst);
  project->addSrcFile(fi.fileName());
  project->save();
}

void MainWindow::closeProject()
{
  if ( project == NULL ) return;
  while ( ui->filesTab[1]->count() > 0 ) {
    closeFileTab1(ui->filesTab[1]->currentIndex());
    }
  while ( ui->filesTab[0]->count() > 0 ) {
    int ind = ui->filesTab[0]->currentIndex();
    QString t = ui->filesTab[0]->tabText(ind);
    if ( project->srcFiles.contains(t) || t.endsWith(Project::fileSuffix) )
      closeFileTab0(ind);
    }
  delete project;
  project = NULL;
  setTreeView(""); // TO FIX !
  updateActions();
}

// EDIT operations 

void MainWindow::keyPressed(int key)
{
  if ( ui->filesTab[0]->count() > 0 ) {
      QWidget* focused = QApplication::focusWidget();
      if( focused != 0 )
        QApplication::postEvent(focused, new QKeyEvent(QEvent::KeyPress, Qt::CTRL+key, Qt::ControlModifier ));
    }
}

void MainWindow::copyText() { keyPressed(Qt::Key_C); }
void MainWindow::cutText() { keyPressed(Qt::Key_X); }
void MainWindow::pasteText() { keyPressed(Qt::Key_V); }
void MainWindow::selectAllText() { keyPressed(Qt::Key_A); }

// VIEW operations 

void MainWindow::setCodeFont()
{
  bool ok;
  QFont font = QFontDialog::getFont(&ok, QFont("Courier", 10), this);
  if ( ok ) {
    foreach (AppFile* f, openedFiles) f->text->setFont(font);
    codeFont = font;
    }
}

void MainWindow::zoomIn()
{
  scaleImage(1.25);
}


void MainWindow::zoomOut()
{
  scaleImage(0.8);
}

void MainWindow::normalSize()
{
  ImageViewer *viewer = selectedImageViewer();
  if ( viewer == NULL ) return;
  viewer->adjustImageSize();
  // updateSelectedTabTitle(); // TODO ? 
}

void MainWindow::fitToWindow()
{
  ImageViewer *viewer = selectedImageViewer();
  if ( viewer == NULL ) return;
  viewer->fitToWindow(ui->actionFitToWindow->isChecked() );
  updateViewActions(viewer);
}

ImageViewer* MainWindow::selectedImageViewer()
{
  int i = ui->filesTab[1]->currentIndex();
  if ( i < 0 ) return NULL;
  QWidget *tab = ui->filesTab[1]->widget(i);
  return tab->whatsThis() == "ImageViewer" ? (ImageViewer *)tab : NULL;
}

void MainWindow::scaleImage(double factor)
{
  ImageViewer *viewer = selectedImageViewer();
  if ( viewer == NULL ) return;
  double newScaleFactor = factor * viewer->getScaleFactor();
  viewer->scaleImage(newScaleFactor);
  ui->actionZoomIn->setEnabled(newScaleFactor < 3.0);
  ui->actionZoomOut->setEnabled(newScaleFactor > 0.33);
  // updateSelectedTabTitle(); // TODO ? 
}

// CONFIG operations

void MainWindow::setGeneralOptions()
{
  //if ( ! Options::getInstance()->exec() ) return;
  options->show(this, "Options");
}

// void clearOptions(void)
// {
// QMap<QString,AppOption> opts = Options::getInstance()->values;
// foreach ( AppOption opt, opts) {
//     switch ( opt.typ ) {
//       case AppOption::UnitOpt:
//         opt.checkbox->setChecked(false);
//         break;
//       case AppOption::StringOpt:
//       case AppOption::IntOpt:
//         opt.val->setText("");
//         break;
//       }
//   }
// }

QString MainWindow::getOptions(QString category, QStringList exclude)
{
  //QMap<QString,AppOption> opts = Options::getInstance()->values;
  QMapIterator<QString, AppOption> i(options->opts);
  QString res;
  while ( i.hasNext() ) {
      i.next();
      AppOption opt = i.value();
      if ( opt.category != category ) continue;
      if ( exclude.contains(opt.name) ) continue;
      if ( opt.checkbox != NULL && opt.checkbox->isChecked() )
        res.append(" " + opt.name);
      else if ( opt.val != NULL && opt.val->text() != "" )
        res.append(" " + opt.name + " " + opt.val->text());
    }
  return res;
}

QString MainWindow::getOption(QString name)
{
  //QMap<QString,AppOption> opts = Options::getInstance()->values;
  Q_ASSERT(options.contains(name));
  AppOption opt = options->opts.value(name);
  switch ( opt.kind ) {
    case AppOption::UnitOpt:
      return opt.checkbox != NULL && opt.checkbox->isChecked() ? "on" : "off";
      break;
    case AppOption::StringOpt:
    case AppOption::IntOpt:
      return opt.val->text();
      break;
    default:
      return "";
    }
}

// BUILD operations

bool MainWindow::executeCmd(QString wDir, QString cmd, bool sync)
{
  bool r = false;
  ui->logText->append("> " + cmd);
  // ui->logText->append("> " + cmd + " [" + wDir + "]");
  proc.setWorkingDirectory(wDir);
  proc.start(cmd);
  if ( proc.error() == QProcess::FailedToStart ) {
    qDebug() << "executeCmd: failed to start" << endl;
    return false;
    }
  if ( sync ) {
    qDebug() << "executeCmd: waiting for process to finish" << endl;
    r = proc.waitForFinished();
    if ( r == true ) r = proc.exitStatus() == QProcess::NormalExit && proc.exitCode() == 0;
    proc.kill();
    proc.close();
    proc.terminate();
    return r;
    }
  else {
    qDebug() << "executeCmd: async process launched" << endl;
    return true;
    }
}

// void removeFiles(QString dirPath, QStringList filePats)
// {
//   QDir dir(dirPath);
//   dir.setNameFilters(filePats);
//   dir.setFilter(QDir::Files);
//   qDebug() << "Removing " << filePats << " in " << dirPath;
//   foreach ( QString dirFile, dir.entryList()) {
//     //qDebug() << "Removing " << dirFile;
//     dir.remove(dirFile);
//     }
// }


void MainWindow::compile(QString type, QString baseCmd, QString targetDir, bool inProject)
{
  // Get source file(s) and working directory
  QString srcFiles, wDir;
  if ( inProject ) {
    if ( project == NULL ) { QMessageBox::warning(this, "", "No opened project"); return; }
    QStringList srcs = project->srcFiles;
    srcFiles = srcs.join(" ");
    wDir = project->dir;
  }
  else {
    if ( ui->filesTab[0]->count() == 0 ) { QMessageBox::warning(this, "", "No input file selected"); return; }
    AppFile *af = indexedFile(0, ui->filesTab[0]->currentIndex());
    if ( af == NULL ) {
      QMessageBox::warning(this, "", "The current selected tab is not a valid input file");
      return;
      }
    QFileInfo f = af->info;
    if ( f.suffix() != "fsm" ) {
      QMessageBox::warning(this, "", "The current selected tab is not a valid input file");
      return;
      }
    srcFiles = f.canonicalFilePath();
    wDir = f.absolutePath();
    }
  QDir dir(wDir);
  // qDebug() << "compile: srcFiles=" << srcFiles << "wDir=" << wDir;
  ui->logText->clear();
  QString compiler = config::getInstance()->getPath("compiler");
  if ( compiler.isNull() || compiler.isEmpty() ) compiler = "rfsmc"; // Last chance..
  if ( targetDir != "" ) dir.mkdir(targetDir);
  // Clean target directory
  // removeFiles(wDir + "/" + targetDir, eraseFirst);
  CommandLine cmd(compiler, baseCmd + " " + srcFiles);
  if ( executeCmd(wDir, cmd.toString()) ) {
    openGeneratedFiles(type, wDir);
    }
  else
    QMessageBox::warning(this, "", "Compilation failed");
  updateActions();
}

void MainWindow::makeDot(bool inProject)
{
  QString targetDir = "-target_dir ./dot";
  QString opts = getOptions("general",specialOptions) + getOptions("dot");
  //QStringList eraseFirst = { "*.dot", "*.gif" };
  compile("dot", " -dot " + targetDir + opts, "dot", inProject);
}

void MainWindow::makeDotFile() { makeDot(false); }
void MainWindow::makeDotProject() { makeDot(true); }
  
void MainWindow::makeCTask(bool inProject)
{
  QString targetDir = "-target_dir ./ctask";
  QString opts = getOptions("general",specialOptions) + getOptions("ctask");
  //QStringList eraseFirst = { "*.c" };
  compile("ctask", " -ctask " + targetDir + opts, "ctask", inProject);
}

void MainWindow::makeCTaskFile() { makeCTask(false); }
void MainWindow::makeCTaskProject() { makeCTask(true); }

void MainWindow::makeSystemC(bool inProject)
{
  QString targetDir = "-target_dir ./systemc";
  QString opts = getOptions("general", specialOptions) + getOptions("systemc");
  //QStringList eraseFirst = { "*.h", "*.cpp" };
  compile("systemc", " -systemc " + targetDir + opts, "systemc", inProject);
}

void MainWindow::makeSystemCFile() { makeSystemC(false); }
void MainWindow::makeSystemCProject() { makeSystemC(true); }
  
void MainWindow::makeVHDL(bool inProject)
{
  QString targetDir = "-target_dir ./vhdl";
  QString opts = getOptions("general", specialOptions) + getOptions("vhdl");
  //QStringList eraseFirst = { "*.vhd" };
  compile("vhdl", " -vhdl " + targetDir + opts, "vhdl", inProject);
}

void MainWindow::makeVHDLFile() { makeVHDL(false); }
void MainWindow::makeVHDLProject() { makeVHDL(true); }

void MainWindow::makeSim()
{
  QString targetDir = "-target_dir ./sim";
  QString opts = getOptions("general", specialOptions) + getOptions("sim");
  //QStringList eraseFirst = { "*.dot", "*.gif" };
  compile("sim", " -sim " + targetDir + opts, "sim", true);
}
  
bool MainWindow::ignore_answer(QString r)
{
  if ( r.isEmpty() ) return true;
  for ( int i=0; i<ignoredAnswerPrefixes.size(); i++ ) 
    if ( r.startsWith(ignoredAnswerPrefixes.at(i)) ) return true;
  return false;
}

void MainWindow::readProcStdout()
{
  proc.setReadChannel(QProcess::StandardOutput);
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! ignore_answer(r) ) ui->logText->append(QString("# ")+r);
      }
}

void MainWindow::readProcStderr()
{
  proc.setReadChannel ( QProcess::StandardError );
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! ignore_answer(r) ) ui->logText->append(QString("# ")+r);
      }
}

QStringList getFileList(QString fname)
{
  QFile ifile(fname);
  QStringList res;
  if ( ! ifile.exists() ) {
      QMessageBox::warning(NULL, "", "Cannot open file " + fname);
      return res;
    }
  ifile.open(QIODevice::ReadOnly | QIODevice::Text);
  QTextStream is(&ifile);
  while( ! is.atEnd() ) {
      QString f = is.readLine(); // One file per line
      res.append(f);
    }
  ifile.close();
  return res;
}

void MainWindow::dotTransform(QFileInfo f, QString wDir)
{
  QString dotProgram = config::getInstance()->getPath("dotProgram");
  if ( dotProgram.isNull() || dotProgram.isEmpty() ) dotProgram = "dot"; // Last chance..
  QString srcFile = f.filePath();
  QString dstFile = changeSuffix(srcFile, ".gif");
  QString opts = getOption("-dot_options");
  CommandLine cmd(dotProgram, opts + " -Tgif -o " + dstFile + " " + srcFile);
  if ( ! executeCmd(wDir, cmd.toString()) )
    QMessageBox::warning(this, "", "Cannot run DOT program");
}

void MainWindow::openGeneratedFiles(QString type, QString dir)
{
  //   while ( ui->outFilesTab->count() > 0 )
  //     ui->outFilesTab->removeTab(ui->outFilesTab->currentIndex());
  QStringList files = getFileList(dir+"/rfsm.output");
  for ( QStringList::ConstIterator file = files.begin(); file != files.end(); file++ ) {
    QFileInfo f(*file);
    if (  (type == "systemc" && (f.suffix() == "cpp" || f.suffix() == "h"))
       || (type == "vhdl" && f.suffix() == "vhd")
       || (type == "ctask" && f.suffix() == "c")
       || (type == "dot" && f.suffix() == "dot") 
       || (type == "sim" && f.suffix() == "vcd") )
    openOutputFile(type, *file, dir);
  }
}

void MainWindow::openOutputFile(QString type, QString fname, QString wDir)
{
  Q_UNUSED(type);
  QFileInfo f(wDir + "/" + fname);
  QString fullName = f.canonicalFilePath();
  QString useDotExternalViewer = getOption("-dot_external_viewer");
  QString useTxtExternalViewer = getOption("-txt_external_viewer");
  qDebug() << "Displaying file : " << fullName;
  if ( f.suffix() == "dot" ) {
    if ( useDotExternalViewer == "on" ) 
      customView("dotViewer", fname, wDir);
    else {
      dotTransform(f, wDir);
      openOutputFile("gif", changeSuffix(fname,".gif"), wDir);
      }  
    }
  else if ( f.suffix() == "vcd" ) {
    QString vcdFileName = changeSuffix(fullName, ".vcd");
    customView("vcdViewer", vcdFileName, wDir);
    }
  else {
    if ( useTxtExternalViewer == "on" && f.suffix() != "gif" ) 
      customView("txtViewer", fname, wDir);
    else
      addFileTab(1, fullName, true, false);
    }
}

void MainWindow::customView(QString toolName, QString fname, QString wDir)
{
   QString toolPath = config::getInstance()->getPath(toolName);
   QFileInfo f(fname);
   QString sName = f.path () + "/" + f.baseName() + ".gtkw";
   QFile sFile(sName);
   QString args = sFile.exists() ? fname + " " + sName : fname;
   if ( ! toolPath.isNull() && ! toolPath.isEmpty() ) {
     CommandLine cmd(toolPath, args);
     qDebug() << "customView cmd: " << cmd.toString() << endl;
     if ( ! executeCmd(wDir, cmd.toString(), false) ) {
         QMessageBox::warning(this, "", "Could not start " + toolName);
         addFileTab(1, fname, true, false);
         }
       }
   else
     QMessageBox::warning(this, "", "No path specified for " + toolName);
}

// Misc

void MainWindow::setPaths()
{
  if ( ! config::getInstance()->exec() ) return;
    writeInitfile();
}

bool MainWindow::alreadyOpened(QString path)
{
  foreach (AppFile* f, openedFiles) {
    if ( f->info.canonicalFilePath() == path ) {
        return true;
      }
    }
  return false;
}

void MainWindow::textHasBeenModified()
{
  int ind = ui->filesTab[0]->currentIndex();
  QString name = ui->filesTab[0]->tabText(ind);
  if ( ! name.endsWith("*") ) {
      name += "*";
      ui->filesTab[0]->setTabText(ind, name);
    }
  if ( openedFiles.contains(ui->filesTab[0]->currentWidget()) ) {
    openedFiles[ui->filesTab[0]->currentWidget()]->upToDate = false;
    }
}

void MainWindow::select(QModelIndex idx)
{
  if ( ! idx.isValid() ) return; // should not happen
  QString path = idx.data(QFileSystemModel::FilePathRole).toString();
  QFileInfo info(path);
  QString suffix = info.completeSuffix();
  if ( ! acceptedSuffixes.contains(suffix) ) return;
  QFileInfo f(path);
  QString fname = f.canonicalFilePath();
  if ( alreadyOpened(fname) ) {
      QMessageBox::warning(this, "Error:", "file:\n" + fname + "\n is already open");
      return;
      }
  addFileTab(0, path, !editableSuffixes.contains(f.suffix()), false);
}

void MainWindow::quit()
{
  closeAllFiles();
  close();
}

void MainWindow::closeEvent(QCloseEvent *)
{
  this->quit();
}

