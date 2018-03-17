#include "config.h"
#include "ui_config.h"

#include <QFileDialog>
#include <QtCore>


config* config::instance = NULL;

config::config(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::config)
{
    ui->setupUi(this);
    connect(ui->compilerPathButton, SIGNAL(clicked()), this, SLOT(setCompilerPath()));
    connect(ui->dotProgramPathButton, SIGNAL(clicked()), this, SLOT(setDotProgramPath()));
    connect(ui->dotViewerPathButton, SIGNAL(clicked()), this, SLOT(setDotViewerPath()));
    connect(ui->vcdViewerPathButton, SIGNAL(clicked()), this, SLOT(setVcdViewerPath()));
    connect(ui->txtViewerPathButton, SIGNAL(clicked()), this, SLOT(setTxtViewerPath()));
}

config::~config()
{
    delete ui;
}

config* config::getInstance ()
{
    if ( instance == NULL )
        instance = new config ();
    return instance;
}

QString getPathName(void)
{
    QString pathname = QFileDialog::getOpenFileName();
    if ( pathname.contains(" ") )  {
      pathname.push_front("\"");
      pathname.push_back("\"");
      }
    return pathname;
}

void config::setCompilerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->compilerPathEdit->setText(path);
}

void config::setDotViewerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->dotViewerPathEdit->setText(path);
}

void config::setDotProgramPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->dotProgramPathEdit->setText(path);
}

void config::setVcdViewerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->vcdViewerPathEdit->setText(path);
}

void config::setTxtViewerPath()
{
    QString path = getPathName();
    if ( ! path.isEmpty() )
      ui->txtViewerPathEdit->setText(path);
}

QString config::getPath(QString name)
{
  if ( name == "compiler" ) return ui->compilerPathEdit->text();
  else if ( name == "dotProgram" ) return ui->dotProgramPathEdit->text();
  else if ( name == "dotViewer" ) return ui->dotViewerPathEdit->text();
  else if ( name == "vcdViewer" ) return ui->vcdViewerPathEdit->text();
  else if ( name == "txtViewer" ) return ui->txtViewerPathEdit->text();
  else return "";
}

void config::setPath(QString name, QString val)
{
  if ( name == "compiler" ) ui->compilerPathEdit->setText(val);
  else if ( name == "dotProgram" ) ui->dotProgramPathEdit->setText(val);
  else if ( name == "dotViewer" ) ui->dotViewerPathEdit->setText(val);
  else if ( name == "vcdViewer" ) ui->vcdViewerPathEdit->setText(val);
  else if ( name == "txtViewer" ) ui->txtViewerPathEdit->setText(val);
}
