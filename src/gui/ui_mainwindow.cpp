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

#include "ui_mainwindow.h"

QT_BEGIN_NAMESPACE

QPushButton* createButton(QWidget *parent, QString name, QString iconName)
{
  QPushButton *button = new QPushButton(parent);
  button->setObjectName(name);
  button->setMinimumSize(QSize(35, 35));
  button->setMaximumSize(QSize(35, 35));
  QIcon icon;
  icon.addFile(iconName, QSize(), QIcon::Normal, QIcon::Off);
  button->setIcon(icon);
  button->setIconSize(QSize(35, 35));
  return button;
}

void Ui_MainWindow::setupUi(QMainWindow *MainWindow)
{
  if (MainWindow->objectName().isEmpty())
    MainWindow->setObjectName(QStringLiteral("MainWindow"));
  MainWindow->resize(800, 630);
  MainWindow->setMinimumSize(QSize(800, 600));

  centralWidget = new QWidget(MainWindow);
  centralWidget->setObjectName(QStringLiteral("centralWidget"));

  vLayout = new QVBoxLayout(centralWidget);
  vLayout->setSpacing(6);
  vLayout->setContentsMargins(11, 11, 11, 11);
  vLayout->setObjectName(QStringLiteral("vLayout"));

  hLayout = new QHBoxLayout();
  hLayout->setSpacing(16);
  hLayout->setObjectName(QStringLiteral("hLayout"));

  openProjectButton = createButton(centralWidget, "openProjectButton", ":/img/open.png");
  hLayout->addWidget(openProjectButton);
  openFileButton = createButton(centralWidget, "openFileButton", ":/img/save.png"); // TO FIX
  hLayout->addWidget(openFileButton);
  newFileButton = createButton(centralWidget, "newFileButton", ":/img/new.png");
  hLayout->addWidget(newFileButton);
  /* saveFileButton = createButton(centralWidget, "saveFileButton", ":/img/save.png"); */
  /* hLayout->addWidget(saveFileButton); */
  /* saveAllButton = createButton(centralWidget, "saveAllButton", ":/img/saveall.png"); */
  /* hLayout->addWidget(saveAllButton); */

  hSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
  hLayout->addItem(hSpacer);

  compileDotFileButton = createButton(centralWidget, "compileDotFileButton", ":/img/compileDotF.png");
  hLayout->addWidget(compileDotFileButton);
  compileCTaskFileButton = createButton(centralWidget, "compileCTaskFilebutton", ":/img/compileCTaskF.png");
  hLayout->addWidget(compileCTaskFileButton);
  compileSystemcFileButton = createButton(centralWidget, "compileSystemcFilebutton", ":/img/compileSystemCF.png");
  hLayout->addWidget(compileSystemcFileButton);
  compileVHDLFileButton = createButton(centralWidget, "compileVHDLFilebutton", ":/img/compileVHDLF.png");
  hLayout->addWidget(compileVHDLFileButton);

  hSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);
  hLayout->addItem(hSpacer);

  compileDotProjectButton = createButton(centralWidget, "compileDotProjectButton", ":/img/compileDot.png");
  hLayout->addWidget(compileDotProjectButton);
  compileCTaskProjectButton = createButton(centralWidget, "compileCTaskProjectButton", ":/img/compileCTask.png");
  hLayout->addWidget(compileCTaskProjectButton);
  compileSystemcProjectButton = createButton(centralWidget, "compileSystemcProjectButton", ":/img/compileSystemC.png");
  hLayout->addWidget(compileSystemcProjectButton);
  compileVHDLProjectButton = createButton(centralWidget, "compileVHDLProjectbutton", ":/img/compileVHDL.png");
  hLayout->addWidget(compileVHDLProjectButton);
  runSimButton = createButton(centralWidget, "runSimButton", ":/img/simu.png");
  hLayout->addWidget(runSimButton);

  vLayout->addLayout(hLayout);

  vSplitter = new QSplitter(centralWidget);
  vSplitter->setObjectName(QStringLiteral("vSplitter"));
  vSplitter->setOrientation(Qt::Vertical);

  hSplitter = new QSplitter(vSplitter);
  hSplitter->setObjectName(QStringLiteral("hSplitter"));

  QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  sizePolicy.setHorizontalStretch(0);
  sizePolicy.setVerticalStretch(0);
  sizePolicy.setHeightForWidth(hSplitter->sizePolicy().hasHeightForWidth());

  hSplitter->setSizePolicy(sizePolicy);
  hSplitter->setMinimumSize(QSize(0, 300));
  hSplitter->setOrientation(Qt::Horizontal);

  treeView = new QTreeView(hSplitter);
  treeView->setObjectName(QStringLiteral("treeView"));
  //treeView->setAlternatingRowColors(true);
  treeView->setSelectionBehavior(QAbstractItemView::SelectItems);
  treeView->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  treeView->setAnimated(false);
  treeView->setAllColumnsShowFocus(true);

  hSplitter->addWidget(treeView);

  filesTab = new QTabWidget(hSplitter);
  filesTab->setObjectName(QStringLiteral("filesTab"));
  QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Expanding);
  sizePolicy1.setHeightForWidth(filesTab->sizePolicy().hasHeightForWidth());
  filesTab->setSizePolicy(sizePolicy1);
  filesTab->setMinimumSize(QSize(500, 150));
  filesTab->setDocumentMode(false);
  filesTab->setTabsClosable(true);
  filesTab->setMovable(true);

  hSplitter->addWidget(filesTab);

  vSplitter->addWidget(hSplitter);

  logText = new QTextEdit(vSplitter);
  logText->setObjectName(QStringLiteral("logText"));
  QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Preferred);
  sizePolicy2.setHorizontalStretch(0);
  sizePolicy2.setVerticalStretch(0);
  sizePolicy2.setHeightForWidth(logText->sizePolicy().hasHeightForWidth());
  logText->setSizePolicy(sizePolicy2);
  logText->setMinimumSize(QSize(0, 50));
  logText->setMaximumSize(QSize(16777215, 150));
  logText->setReadOnly(true);

  vSplitter->addWidget(logText);

  vLayout->addWidget(vSplitter);

  MainWindow->setCentralWidget(centralWidget);
  menuBar = new QMenuBar(MainWindow);
  menuBar->setObjectName(QStringLiteral("menuBar"));
  menuBar->setGeometry(QRect(0, 0, 800, 22));
  MainWindow->setMenuBar(menuBar);

  retranslateUi(MainWindow);

  QMetaObject::connectSlotsByName(MainWindow);
} 

void Ui_MainWindow::createMenus(QMainWindow *MainWindow)
{
  menuBar->clear();

  QMenu *fileMenu = menuBar->addMenu("&File");

  actionNewFile = fileMenu->addAction("&New file");
  actionOpenFile = fileMenu->addAction("&Open file");
  actionSaveCurrentFile = fileMenu->addAction("&Save current file");
  actionSaveCurrentFileAs = fileMenu->addAction("&Save current file as");
  actionCloseFile = fileMenu->addAction("&Close file");
  actionCloseAllFiles = fileMenu->addAction("&Close all files");
  fileMenu->addSeparator();
  actionAbout = fileMenu->addAction("&About");
  actionQuit = fileMenu->addAction("&Quit");

  actionNewFile->setShortcut(QKeySequence("Ctrl+N"));
  // actionSaveFile->setShortcut(QKeySequence("Ctrl+S"));
  actionQuit->setShortcut(QKeySequence("Ctrl+Q"));

  actionSaveCurrentFile->setEnabled(false);
  actionSaveCurrentFileAs->setEnabled(false);
  actionCloseFile->setEnabled(false);
  actionCloseAllFiles->setEnabled(false);

  QMenu *projectMenu = menuBar->addMenu("&Project");

  actionNewProject = projectMenu->addAction("&New project");
  actionOpenProject = projectMenu->addAction("&Open project");
  // actionAddCurrentFileToProject = projectMenu->addAction("&Add current file to project");
  actionAddFileToProject = projectMenu->addAction("&Add file to project");
  actionEditProject = projectMenu->addAction("&Edit project");
  actionSaveProject = projectMenu->addAction("&Save project");
  actionSaveProjectAs = projectMenu->addAction("&Save project as");
  actionCloseProject = projectMenu->addAction("&Close project");

  actionAddFileToProject->setEnabled(false);
  actionEditProject->setEnabled(false);
  actionCloseProject->setEnabled(false);

  actionOpenProject->setShortcut(QKeySequence("Ctrl+O"));

  QMenu *buildMenu = menuBar->addMenu("Build");
  actionBuildDotFile = buildMenu->addAction("&Build DOT representation for file");
  actionBuildCTaskFile = buildMenu->addAction("&Build C code for file");
  actionBuildSystemCFile = buildMenu->addAction("&Build SystemC code for file");
  actionBuildVHDLFile = buildMenu->addAction("&Build VHDL code for file");
  buildMenu->addSeparator();
  actionBuildDotProject = buildMenu->addAction("&Build DOT representation for project");
  actionBuildCTaskProject = buildMenu->addAction("&Build C code for project");
  actionBuildSystemCProject = buildMenu->addAction("&Build SystemC code for project");
  actionBuildVHDLProject = buildMenu->addAction("&Build VHDL code for project");
  actionRunSim = buildMenu->addAction("&Run simulation for project");

  QMenu *editMenu = menuBar->addMenu("&Edit");

  actionCopy = editMenu->addAction("&Copy");
  actionCut = editMenu->addAction("&Cut");
  actionPaste = editMenu->addAction("&Paste");
  actionSelect = editMenu->addAction("&Select all");

  // QString ICON_NEW(":/img/new.png");
  // QString ICON_OPEN( ":/img/open.png");
  // QString ICON_SAVE( ":/img/save.png");
  // QString ICON_SAVEALL( ":/img/saveall.png");
  // QString ICON_QUIT(":/img/exit.png");
  // QString ICON_COPY(":/img/copier.png");
  // QString ICON_CUT(":/img/couper.png");
  // QString ICON_PASTE(":/img/coller.png");
  // QString ICON_SELECT(":/img/select_all.png");
  // QString ICON_COLOR(":/img/color.png");
  // QString ICON_PREF(":/img/preferences.png");
  // QString ICON_CLOSEF(":/img/closefile.png");

  // actionOpenProject->setIcon(QIcon(ICON_OPEN));
  // actionNewFile->setIcon(QIcon(ICON_NEW));
  // // actionSaveFile->setIcon(QIcon(ICON_SAVE));
  // // actionSaveAll->setIcon(QIcon(ICON_SAVEALL));
  // actionQuit->setIcon(QIcon(ICON_QUIT));
  // actionCopy->setIcon(QIcon(ICON_COPY));
  // actionCut->setIcon(QIcon(ICON_CUT));
  // actionPaste->setIcon(QIcon(ICON_PASTE));
  // actionSelect->setIcon(QIcon(ICON_SELECT));
  // actionCloseFile->setIcon(QIcon(ICON_CLOSEF));

  QMenu *viewMenu = menuBar->addMenu("View");

  actionZoomIn = viewMenu->addAction("Zoom &In");
  actionZoomOut = viewMenu->addAction("Zoom &Out");
  actionNormalSize = viewMenu->addAction("&Normal Size (100%)");
  viewMenu->addSeparator();
  actionFitToWindow = viewMenu->addAction("Fit to Window");
  actionZoomIn->setShortcut(QString("Ctrl++"));
  actionZoomOut->setShortcut(QString("Ctrl+-"));
  actionFitToWindow->setShortcut(QString("Ctrl+F"));
  actionFitToWindow->setCheckable(true);

  QMenu *menuConfig = menuBar->addMenu("Configuration");

  actionPathConfig = menuConfig->addAction("&Compiler and tools");
  actionGeneralOptions = menuConfig->addAction("&Compiler options");
  actionFontConfig = menuConfig->addAction("&Code font");
}

QT_END_NAMESPACE
