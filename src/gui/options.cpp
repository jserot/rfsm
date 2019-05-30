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

#include <QtWidgets>
#include <QFile>
#include <QMessageBox>

#include "option.h"
#include "options.h"

Options::Options(QString specFile, QWidget *parent)
{
  QFile file(specFile);
  file.open(QIODevice::ReadOnly);
  if ( file.error() != QFile::NoError ) {
    QMessageBox::warning(parent, "","Cannot read specification file " + file.fileName());
    return;
  }
  while ( ! file.atEnd() ) {
    QString line = file.readLine();
    QStringList items = line.split(";");
    // 0: IDE flag, 1: category, 2: name, 3: kind, 4: desc
    if ( items.length() < 6 || items[0] != "ide" ) continue;
    if ( items[0] != "ide" ) continue;
    if ( items[3] == "Arg.Unit" ) {
      AppOption opt(items[1], items[2], AppOption::UnitOpt, items[5]);
      opts.insert(items[2], opt);
    }
    else if ( items[3] == "Arg.String" ) {
      AppOption opt(items[1], items[2], AppOption::StringOpt, items[5]);
      opts.insert(items[2], opt);
    }
    else if ( items[3] == "Arg.Int" ) {
      AppOption opt(items[1], items[2], AppOption::IntOpt, items[5]);
      opts.insert(items[2], opt);
    }
    else { /* TO FIX */ }
  }
  file.close();
}

QString Options::readFromProject(Project *project)
{
  QString r;
  // An empty return string means Ok, otherwise the string contains the error message
  // This is the poor man (aka C++) implementation of Caml option type..
  if ( project == NULL ) return "";
  if ( (r = parse_opts("dot", project->dotOptions)) != "" ) return r;
  if ( (r = parse_opts("ctask", project->ctaskOptions)) != "" ) return r;
  if ( (r = parse_opts("systemc", project->systemcOptions)) != "" ) return r;
  if ( (r = parse_opts("vhdl", project->vhdlOptions)) != "" ) return r;
  if ( (r = parse_opts("sim", project->simOptions)) != "" ) return r;
  qDebug() << "Updated options with settings from project file " << project->file;
  return "";
}

QString Options::parse_opts(QString cat, QString line)
// Parsing .pro text spec. Ex: "-dot_xxx -dot_yyy arg ..."
{
  line = line.trimmed();
  if ( line.isEmpty() ) return "";
  QStringList items = line.split(" ");
  for ( int i=0; i<items.length(); i++ ) {
    QString k = items.at(i);
    if ( k.isEmpty() ) continue;
    if ( k.startsWith("#") ) break;
    if ( ! opts.contains(k) ) return ("unrecognized option: \"" + k + "\"");
    AppOption opt = opts.value(k);
    if ( opt.category != cat ) return ("option " + k + " does not belong to category " + cat) ;
    switch ( opt.kind ) {
      case AppOption::UnitOpt:
        opt.checkbox->setChecked(true);
        break;
      case AppOption::StringOpt:
      case AppOption::IntOpt:
        ++i;
        if ( i < items.length() ) 
          opt.val->setText(items.at(i));
        else
          return ("missing argument for option " + k);
        break;
      }
    }
  return "";
}

void Options::show(QWidget *parent, QString title)
{
  QDialog *dialog = new QDialog(parent);
  QTabWidget* tabs = new QTabWidget(dialog);
  addTab("General", "general", tabs, parent);
  addTab("Dot", "dot", tabs, parent);
  addTab("Sim", "sim", tabs, parent);
  addTab("SystemC", "systemc", tabs, parent);
  addTab("VHDL", "vhdl", tabs, parent);
  QVBoxLayout *mainLayout = new QVBoxLayout();
  mainLayout->addWidget(tabs);
  QDialogButtonBox *buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
  dialog->connect(buttonBox, SIGNAL(accepted()), dialog, SLOT(accept()));
  dialog->connect(buttonBox, SIGNAL(rejected()), dialog, SLOT(reject()));
  mainLayout->addWidget(buttonBox);
  dialog->setLayout(mainLayout);
  dialog->setWindowTitle(title);
  dialog->exec();
}

void Options::addTab(QString title, QString category, QTabWidget *tabs, QWidget *parent)
{
    QWidget *container = new QWidget(parent);
    QFormLayout *layout = new QFormLayout(container);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    QList<AppOption> vs = opts.values();
    for ( int i=0; i<vs.length(); i++ ) {
      AppOption opt = vs.at(i);
      if ( opt.category != category ) continue;
      switch ( opt.kind ) {
        case AppOption::UnitOpt:
            layout->addRow(new QLabel(opt.desc), opt.checkbox);
            break;
        case AppOption::StringOpt:
            layout->addRow(new QLabel(opt.desc), opt.val);
            break;
        case AppOption::IntOpt:
            layout->addRow(new QLabel(opt.desc), opt.val);
            break;
        }
      }
    container->setLayout(layout);
    QScrollArea *scroll = new QScrollArea(parent);
    scroll->setWidget(container);
    tabs->addTab(scroll, title);
}

Options::~Options()
{
}
