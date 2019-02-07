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

MainWindow::MainWindow(QWidget *parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow)
{
  ui->setupUi(this);

  setWindowIcon(QPixmap( ":/img/icon.png" ));
  createViewActions();
  createMenus();
  ui->inpFilesTab->setMovable(false);

  ui->inpFilesTab->removeTab(0);  // TO FIX : only one tab at startup ?
  ui->inpFilesTab->removeTab(0);
  ui->outFilesTab->removeTab(0);
  ui->outFilesTab->removeTab(0);

  connect(ui->newFileButton, SIGNAL(clicked()), this, SLOT(newFile()));
  connect(ui->openFileButton, SIGNAL(clicked()), this, SLOT(openFile()));
  connect(ui->saveFileButton, SIGNAL(clicked()), this, SLOT(saveFile()));
  connect(ui->saveAllButton, SIGNAL(clicked()), this, SLOT(saveAll()));

  connect(ui->compileDotButton, SIGNAL(clicked()), this, SLOT(makeDot()));
  connect(ui->runSimButton, SIGNAL(clicked()), this, SLOT(makeSim()));
  connect(ui->compileCTaskButton, SIGNAL(clicked()), this, SLOT(makeCTask()));
  connect(ui->compileSystemcButton, SIGNAL(clicked()), this, SLOT(makeSystemC()));
  connect(ui->compileVHDLButton, SIGNAL(clicked()), this, SLOT(makeVHDL()));

  connect(&proc,SIGNAL(readyReadStandardOutput ()),this,  SLOT(readProcStdout()));
  connect(&proc,SIGNAL(readyReadStandardError ()),this,  SLOT(readProcStderr()));

  connect(ui->inpFilesTab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeInputFileTab(int)));
  connect(ui->outFilesTab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeOutputFileTab(int)));
  connect(ui->outFilesTab, SIGNAL(currentChanged(int)), this, SLOT(outTabChanged(int)));

  ui->logText->setReadOnly(true);

  codeFont.setFamily("Courier");
  codeFont.setFixedPitch(true);
  codeFont.setPointSize(11);

  readInitFile();
}

void MainWindow::about()
{
  QMessageBox::about(this, tr("About RFSM"),  // TO FIX
                     tr("<p>Contact: jocelyn.serot@uca.fr</p>"));
}

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

MainWindow::~MainWindow()
{
  delete ui;
}

void MainWindow::createMenus()
{
  menuBar()->clear();
  QMenu *menuFichier = menuBar()->addMenu("&File");
  QMenu *menuEdition = menuBar()->addMenu("&Edit");

  QAction *actionNouvFich = menuFichier->addAction("&New file");
  QAction *actionOuvrirFich = menuFichier->addAction("&Open file");
  menuFichier->addSeparator();
  QAction *actionSaveFich = menuFichier->addAction("&Save");
  QAction *actionSaveFichAs = menuFichier->addAction("&Save as");
  QAction *actionSaveAll = menuFichier->addAction("&Save all");
  menuFichier->addSeparator();
  QAction *actionCloseFile = menuFichier->addAction("&Close file");
  QAction *actionCloseAll = menuFichier->addAction("&Close all");
  menuFichier->addSeparator();
  QAction *actionQuit = menuFichier->addAction("&Quit");
  QAction *actionCopy = menuEdition->addAction("&Copy");
  QAction *actionCut = menuEdition->addAction("&Cut");
  QAction *actionPaste = menuEdition->addAction("&Paste");
  QAction *actionSelect = menuEdition->addAction("&Select all");

  QObject::connect(actionNouvFich, SIGNAL(triggered()), this, SLOT(newFile()));
  QObject::connect(actionOuvrirFich, SIGNAL(triggered()), this, SLOT(openFile()));
  QObject::connect(actionSaveFich, SIGNAL(triggered()), this, SLOT(saveFile()));
  QObject::connect(actionSaveFichAs, SIGNAL(triggered()), this, SLOT(saveFileAs()));
  QObject::connect(actionSaveAll, SIGNAL(triggered()), this, SLOT(saveAll()));
  QObject::connect(actionCloseFile, SIGNAL(triggered()), this, SLOT(closeCurrentFile()));
  QObject::connect(actionCloseAll, SIGNAL(triggered()), this, SLOT(closeAll()));
  QObject::connect(actionQuit, SIGNAL(triggered()), this, SLOT(quit()));

  QObject::connect(actionCopy, SIGNAL(triggered()), this, SLOT(copyText()));
  QObject::connect(actionCut, SIGNAL(triggered()), this, SLOT(cutText()));
  QObject::connect(actionPaste, SIGNAL(triggered()), this, SLOT(pasteText()));
  QObject::connect(actionSelect, SIGNAL(triggered()), this, SLOT(selectAllText()));

  actionSaveFich->setShortcut(QKeySequence("Ctrl+S"));
  actionNouvFich->setShortcut(QKeySequence("Ctrl+N"));
  actionOuvrirFich->setShortcut(QKeySequence("Ctrl+O"));
  actionQuit->setShortcut(QKeySequence("Ctrl+Q"));
  actionSaveAll->setShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT +Qt::Key_S ));

  QString ICON_NEW(":/img/new.png");
  QString ICON_OPEN( ":/img/open.png");
  QString ICON_SAVE( ":/img/save.png");
  QString ICON_SAVEALL( ":/img/saveall.png");
  QString ICON_QUIT(":/img/exit.png");
  QString ICON_COPY(":/img/copier.png");
  QString ICON_CUT(":/img/couper.png");
  QString ICON_PASTE(":/img/coller.png");
  QString ICON_SELECT(":/img/select_all.png");
  QString ICON_COLOR(":/img/color.png");
  QString ICON_PREF(":/img/preferences.png");
  QString ICON_CLOSEF(":/img/closefile.png");

  actionNouvFich->setIcon(QIcon(ICON_NEW));
  actionOuvrirFich->setIcon(QIcon(ICON_OPEN));
  actionSaveFich->setIcon(QIcon(ICON_SAVE));
  actionSaveAll->setIcon(QIcon(ICON_SAVEALL));
  actionQuit->setIcon(QIcon(ICON_QUIT));
  actionCopy->setIcon(QIcon(ICON_COPY));
  actionCut->setIcon(QIcon(ICON_CUT));
  actionPaste->setIcon(QIcon(ICON_PASTE));
  actionSelect->setIcon(QIcon(ICON_SELECT));
  actionCloseFile->setIcon(QIcon(ICON_CLOSEF));

  QMenu *menuView = menuBar()->addMenu("View");
  menuView->addAction(zoomInAct);
  menuView->addAction(zoomOutAct);
  menuView->addAction(normalSizeAct);
  menuView->addSeparator();
  menuView->addAction(fitToWindowAct);

  QMenu *menuConfig = menuBar()->addMenu("Configuration");
  QAction *pathConfig = menuConfig->addAction("&Compiler and tools");
  QAction *generalOptions = menuConfig->addAction("&Compiler options");
  QAction *fontConfig = menuConfig->addAction("&Code font");
  QObject::connect(pathConfig,SIGNAL(triggered()),this,SLOT(setPaths()));
  QObject::connect(generalOptions, SIGNAL(triggered()), this, SLOT(setGeneralOptions()));
  QObject::connect(fontConfig, SIGNAL(triggered()), this, SLOT(setCodeFont()));
}

void MainWindow::createViewActions()
{
  zoomInAct = new QAction(QString("Zoom &In"), this);
  zoomInAct->setShortcut(tr("Ctrl++"));
  zoomInAct->setEnabled(false);
  connect(zoomInAct, SIGNAL(triggered()), this, SLOT(zoomIn()));

  zoomOutAct = new QAction(QString("Zoom &Out"), this);
  zoomOutAct->setShortcut(tr("Ctrl+-"));
  zoomOutAct->setEnabled(false);
  connect(zoomOutAct, SIGNAL(triggered()), this, SLOT(zoomOut()));

  normalSizeAct = new QAction(tr("&Normal Size (100%)"), this);
  normalSizeAct->setShortcut(tr("Ctrl+0"));
  normalSizeAct->setEnabled(false);
  connect(normalSizeAct, SIGNAL(triggered()), this, SLOT(normalSize()));

  fitToWindowAct = new QAction(tr("&Fit to Window"), this);
  fitToWindowAct->setEnabled(false);
  fitToWindowAct->setCheckable(true);
  fitToWindowAct->setShortcut(tr("Ctrl+F"));
  connect(fitToWindowAct, SIGNAL(triggered()), this, SLOT(fitToWindow()));

  // aboutQtAct = new QAction(tr("About &Qt"), this);
  // connect(aboutQtAct, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

  // helpAct = new QAction(tr("&Help"), this);
  // helpAct->setShortcut(tr("F1"));
  // connect(helpAct, SIGNAL(triggered()), this, SLOT(helpDialog()));
}


void MainWindow::openFile()
{
  QString filename = QFileDialog::getOpenFileName(this, "Open file", initDir, "*.fsm");
  if ( filename.isEmpty() ) return;
  QFile file(filename);
  if ( filename.right(4) == ".fsm" ) {
    if ( alreadyOpened(filename) ) {
      QMessageBox::warning(this, "Error:", "file:\n" + filename + "\n is already open");
      return;
      }
    addFileTab(filename, ui->inpFilesTab, inFiles);
  }
  else
    addFileTab(filename, ui->outFilesTab, outFiles);
}

void MainWindow::newFile()
{
  QTemporaryFile* f = new QTemporaryFile(QDir::tempPath()+"/rfsmTmp_XXXXXX.fsm");  // TODO : delete when saved..
  f->open();
  qDebug() << "Creating tmp file " << f->fileName();
  addFileTab(f->fileName(), ui->inpFilesTab, inFiles, false, true);
}

void MainWindow::saveFile()
{
  if ( ui->inpFilesTab->count() !=0 ) {
      int ind = ui->inpFilesTab->currentIndex();
      saveIndexedFile(ind);
      ui->runSimButton->setEnabled(false);
      this->repaint();
      ui->runSimButton->setEnabled(true);
      this->repaint();
    }
}

void MainWindow::saveAll()
{
  for ( int ind=0; ind<ui->inpFilesTab->count(); ind++ ) {
      ui->inpFilesTab->setCurrentIndex(ind);
      saveIndexedFile(ind);
    }
}

void MainWindow::saveIndexedFile(int ind)
{
  QString path = inFiles[ind]->path;
  qDebug() << "Saving " << path;
  if ( path.contains("rfsmTmp") ) {
      path = QFileDialog::getSaveFileName(this,"Select location to save file");
      if ( path.isEmpty() ) return;
      qDebug() << "Saving as: " << path;
      //inFiles[ui->inpFilesTab->currentIndex()].upToDate = false; // ???
      inFiles[ind]->path = path;
      QFileInfo f(path);
      ui->inpFilesTab->setTabText(ind, f.fileName());
    }
  QFile save(path);
  if ( ! save.open(QFile::WriteOnly | QFile::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+ path + " for writing");
      return;
    }
  QTextStream os(&save);
  QTextEdit* text = inFiles[ind]->text;
  os << text->toPlainText();
  save.flush();
  save.close();
  QString nouv = ui->inpFilesTab->tabText(ind);
  if ( nouv.at(nouv.size()-1)=='*' ) {
      nouv.replace("*","");
      ui->inpFilesTab->setTabText(ind,nouv);
    }
  inFiles[ui->inpFilesTab->currentIndex()]->upToDate = true;
}

void MainWindow::closeIndexedFile(int ind)
{
  if ( inFiles[ind]->upToDate == false ) {
      QMessageBox msgBox;
      msgBox.setText("File unsaved.");
      msgBox.setInformativeText("Do you want to save your changes ?");
      msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard);
      msgBox.setDefaultButton(QMessageBox::Save);
      switch ( msgBox.exec() ) {
        case QMessageBox::Save:
          saveIndexedFile(ind);
          break;
        case QMessageBox::Discard:
          break;
        default:
          break;
        }
    }
  delete inFiles[ind];
  inFiles.removeAt(ind);
  ui->inpFilesTab->removeTab(ind);
}

void MainWindow::closeCurrentFile()
{
  if ( ui->inpFilesTab->count() > 0 )
    closeIndexedFile(ui->inpFilesTab->currentIndex());
}

void MainWindow::closeAll()
{
  while ( ui->inpFilesTab->count() > 0 )
    closeCurrentFile();
}

void MainWindow::saveFileAs()
{
  int ind = ui->inpFilesTab->currentIndex();
  inFiles[ind]->upToDate = false;
  QString path = QFileDialog::getSaveFileName(this,"Select location to save file");
  if ( path.isEmpty() ) return;
  qDebug() << "Saving as: " << path;
  inFiles[ind]->path = path;
  saveIndexedFile(ind);
  QFileInfo f(path);
  ui->inpFilesTab->setTabText(ind, f.fileName());
  inFiles[ind]->upToDate = true;
}

void MainWindow::closeInputFileTab(int index)
{
  closeIndexedFile(index);
}

void MainWindow::closeOutputFileTab(int index)
{
  ui->outFilesTab->removeTab(index);
}

void MainWindow::outTabChanged(int index)
{
  updateViewActions();
}

void MainWindow::keyPressed(int key)
{
  if ( ui->inpFilesTab->count() > 0 ) {
      QWidget* focused = QApplication::focusWidget();
      if( focused != 0 )
        QApplication::postEvent(focused, new QKeyEvent(QEvent::KeyPress, Qt::CTRL+key, Qt::ControlModifier ));
    }
}

void MainWindow::copyText() { keyPressed(Qt::Key_C); }
void MainWindow::cutText() { keyPressed(Qt::Key_X); }
void MainWindow::pasteText() { keyPressed(Qt::Key_V); }
void MainWindow::selectAllText() { keyPressed(Qt::Key_A); }

void MainWindow::quit()
{
  closeAll();
  close();
}

void MainWindow::closeEvent(QCloseEvent *)
{
  this->quit();
}

void MainWindow::setGeneralOptions()
{
  if ( ! Options::getInstance()->exec() ) return;
}

void clearOptions(void)
{
QMap<QString,AppOption> opts = Options::getInstance()->values;
foreach ( AppOption opt, opts) {
    switch ( opt.typ ) {
      case AppOption::UnitOpt:
        opt.checkbox->setChecked(false);
        break;
      case AppOption::StringOpt:
      case AppOption::IntOpt:
        opt.val->setText("");
        break;
      }
  }
}

QString getOptions(QString category, QStringList exclude=QStringList())
{
  QMap<QString,AppOption> opts = Options::getInstance()->values;
  QMapIterator<QString, AppOption> i(opts);
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

QString getOption(QString name)
{
  QMap<QString,AppOption> opts = Options::getInstance()->values;
  Q_ASSERT(opts.contains(name));
  AppOption opt = opts.value(name);
  switch ( opt.typ ) {
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

bool MainWindow::executeCmd(QString wDir, QString cmd, bool sync)
{
  bool r = true;
  ui->logText->append("> " + cmd);
  proc.setWorkingDirectory(wDir);
  proc.start(cmd);
  if ( proc.error() == QProcess::FailedToStart ) r = false;
  else if ( sync ) {
    r = proc.waitForFinished();
    if ( r == true ) r = proc.exitStatus() == QProcess::NormalExit;
    proc.kill();
    proc.close();
    proc.terminate();
    }
  return r;
}

void MainWindow::compile(QString type, QString baseCmd, QString targetDir)
{
  ui->logText->clear();
  if( ui->inpFilesTab->count() == 0 ) return;
  saveFile();
  QString compiler = config::getInstance()->getPath("compiler");
  if ( compiler.isNull() || compiler.isEmpty() ) compiler = "rfsm"; // Last chance..
  // Get actual source file
  QString srcFilePath = ui->inpFilesTab->count() > 0 ? inFiles[ui->inpFilesTab->currentIndex()]->path : "";
  if ( srcFilePath == "" ) return;
  QFileInfo f(srcFilePath);
  QDir dir = f.absoluteDir();
  if ( targetDir != "" ) dir.mkdir(targetDir);
  QString srcFile = f.fileName();
  QString wDir = dir.absolutePath();
  CommandLine cmd(compiler, baseCmd + " " + srcFile);
  if ( executeCmd(wDir, cmd.toString()) ) {
    openGeneratedFiles(type, wDir);
    }
  else
    QMessageBox::warning(this, "", "Compilation failed");
}

void MainWindow::makeDot()
{
  QString targetDir = "-target_dir ./dot";
  QStringList exclude("-dot_options");
  QString opts = getOptions("general") + getOptions("dot",exclude);
  compile("dot", " -dot " + targetDir + opts, "dot");
}

void MainWindow::makeSim()
{
  QString opts = getOptions("general") + getOptions("sim");
  compile("simu", " -sim -vcd ./sim/run.vcd" + opts, "sim");
}

void MainWindow::makeCTask()
{
  QString targetDir = "-target_dir ./ctask";
  QString opts = getOptions("general") + getOptions("ctask");
  compile("ctask", " -ctask " + targetDir + opts, "ctask");
}

void MainWindow::makeSystemC()
{
  QString targetDir = "-target_dir ./systemc";
  QString opts = getOptions("general") + getOptions("systemc");
  compile("systemc", " -systemc " + targetDir + opts, "systemc");
}

void MainWindow::makeVHDL()
{
  QString targetDir = "-target_dir ./vhdl";
  QString opts = getOptions("general") + getOptions("vhdl");
  compile("vhdl", " -vhdl " + targetDir + opts, "vhdl");
}

void MainWindow::readProcStdout()
{
  proc.setReadChannel(QProcess::StandardOutput);
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! r.isEmpty() && ! r.startsWith("** (ImageGlass") ) ui->logText->append(QString("# ")+r);
      }
}

void MainWindow::readProcStderr()
{
  proc.setReadChannel ( QProcess::StandardError );
  while (proc.canReadLine ()) {
      QString r = QString(proc.readLine()).remove('\n').remove ('\r');
      if ( ! r.isEmpty() && ! r.startsWith("** (ImageGlass") ) ui->logText->append(QString("# ")+r);
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

QString changeSuffix(QString fname, QString suffix)
{
  QFileInfo f(fname);
  return f.path() + "/" + f.completeBaseName() + suffix;
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
       || (type == "simu" && f.suffix() == "vcd") )
    openOutputFile(type, *file, dir);
  }
}

void MainWindow::openOutputFile(QString type, QString fname, QString wDir)
{
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
      addFileTab(fullName, ui->outFilesTab, outFiles, true, false);
    }
}

void MainWindow::customView(QString toolName, QString fname, QString wDir)
{
   QString toolPath = config::getInstance()->getPath(toolName);
   if ( ! toolPath.isNull() && ! toolPath.isEmpty() ) {
     CommandLine cmd (toolPath, fname);
     if ( ! executeCmd(wDir, cmd.toString(), false) ) {
         QMessageBox::warning(this, "", "Could not start " + toolName);
         addFileTab(fname, ui->outFilesTab, outFiles, true, false);
         }
       }
   else
     QMessageBox::warning(this, "", "No path specified for " + toolName);
}


void MainWindow::addFileTab(QString fname, QTabWidget *ui_tabs, QList<AppFile*>& files, bool ronly, bool isTemp)
{
  QFile file(fname);
  QFileInfo f(fname);
  if ( ! file.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      QMessageBox::warning(this,"Error:","cannot open file:\n"+fname);
      return;
    }
  if ( fname.right(4) == ".gif" ) {
    QPixmap pixmap(f.filePath());
    ImageViewer *viewer = new ImageViewer();
    viewer->setPixmap(pixmap);
    viewer->setWhatsThis("ImageViewer");
    ui_tabs->addTab(viewer, changeSuffix(f.fileName(),".dot"));
    viewer->adjustImageSize();
    } 
  else {
    QTextEdit* edit = new QTextEdit();
#ifdef Q_OS_MACOS
    edit->setFont(codeFont);
#endif
    edit->setWhatsThis("TextEditor");
    edit->setPlainText(QString::fromUtf8(file.readAll()));
    edit->setReadOnly(ronly);
    if ( ! ronly ) QObject::connect(edit, SIGNAL(textChanged()), this, SLOT(textHasBeenModified()));
    ui_tabs->addTab(edit, isTemp ? "new" : f.fileName());
    SyntaxHighlighter* highlighter;
    if ( f.suffix() == "fsm" ) highlighter = new FsmSyntaxHighlighter(edit->document());
    else if ( f.suffix() == "c" ) highlighter = new CTaskSyntaxHighlighter(edit->document());
    else highlighter = NULL;
    AppFile* fic = new AppFile(fname, true, edit, highlighter);
    files.append(fic);
    }
  updateViewActions();
  ui_tabs->setCurrentIndex(ui_tabs->count()-1);
}


void writeInifile(void)
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

void MainWindow::setPaths()
{
  if ( ! config::getInstance()->exec() ) return;
    writeInifile();
}

bool MainWindow::alreadyOpened(QString path)
{
  for ( int i=0; i<inFiles.length(); i++ )
    if ( inFiles[i]->path == path ) {
        return true;
      }
  return false;
}

void MainWindow::textHasBeenModified()
{
  int ind = ui->inpFilesTab->currentIndex();
  QString nouv = ui->inpFilesTab->tabText(ind);
  if ( nouv.at(nouv.size()-1)!='*' ) {
      nouv += "*";
      ui->inpFilesTab->setTabText(ind,nouv);
    }
  inFiles[ind]->upToDate = false;
}

void MainWindow::setCodeFont()
{
  bool ok;
  int i;
  QFont font = QFontDialog::getFont(&ok, QFont("Courier", 10), this);
  if ( ok ) {
      for ( i=0; i<inFiles.length(); i++ )
        inFiles[i]->text->setFont(font);
      for ( i=0; i<outFiles.length(); i++ )
        outFiles[i]->text->setFont(font);
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
  viewer->fitToWindow( fitToWindowAct->isChecked() );
  updateViewActions();
}

ImageViewer* MainWindow::selectedImageViewer()
{
  int i = ui->outFilesTab->currentIndex();
  if ( i < 0 ) return NULL;
  QWidget *tab = ui->outFilesTab->widget(i);
  return tab->whatsThis() == "ImageViewer" ? (ImageViewer *)tab : NULL;
}

void MainWindow::scaleImage(double factor)
{
  ImageViewer *viewer = selectedImageViewer();
  if ( viewer == NULL ) return;
  double newScaleFactor = factor * viewer->getScaleFactor();
  viewer->scaleImage(newScaleFactor);
  zoomInAct->setEnabled(newScaleFactor < 3.0);
  zoomOutAct->setEnabled(newScaleFactor > 0.33);
  // updateSelectedTabTitle(); // TODO ? 
}

void MainWindow::updateViewActions()
{
  ImageViewer *viewer = selectedImageViewer();
  if ( viewer ) {
    bool b = viewer->isFittedToWindow();
    fitToWindowAct->setEnabled(true);
    fitToWindowAct->setChecked(b);
    zoomInAct->setEnabled(!b);
    zoomOutAct->setEnabled(!b);
    normalSizeAct->setEnabled(!b);
    }
  else {
    fitToWindowAct->setEnabled(false);
    zoomInAct->setEnabled(false);
    zoomOutAct->setEnabled(false);
    normalSizeAct->setEnabled(false);
    }
}
