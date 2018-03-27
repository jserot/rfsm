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

#include "option.h"
#include "options.h"

Options::Options(QWidget *parent)
    : QDialog(parent)
{
    QFile file(":/options_spec.txt");
    file.open(QIODevice::ReadOnly);
    if ( file.error() != QFile::NoError ) {
        QMessageBox::warning(this, "","Error reading file " + file.fileName());
        return;
        }
    file.close();
    tabs = new QTabWidget(parent);
    addTab("General", "general");
    addTab("Dot", "dot");
    addTab("Simu", "simu");
    addTab("SystemC", "systemc");
    addTab("VHDL", "vhdl");
    QVBoxLayout *mainLayout = new QVBoxLayout();
    mainLayout->addWidget(tabs);
    buttonBox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));
    connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject()));
    mainLayout->addWidget(buttonBox);
    setLayout(mainLayout);
    setWindowTitle(tr("Options"));
}

void Options::addTab(QString title, QString category)
{
    QWidget *container = new QWidget(this);
    QFormLayout *layout = new QFormLayout(container);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    QFile file(":options_spec.txt");
    file.open(QIODevice::ReadOnly);
    if ( file.error() != QFile::NoError ) return;

    while ( ! file.atEnd() ) {
        QString line = file.readLine();
        QStringList items = line.split(";");
        if ( items.length() < 6 || items[0] != "ide" ) continue;
        if ( items[0] != "ide" ) continue;
        if ( items[1] != category ) continue;
        if ( items[3] == "Arg.Unit" ) {
            AppOption opt(items[1], items[2], AppOption::UnitOpt, items[5]);
            layout->addRow(new QLabel(opt.desc), opt.checkbox);
            values.insert(items[2], opt);
        }
        else if ( items[3] == "Arg.String" ) {
            AppOption opt(items[1], items[2], AppOption::StringOpt, items[5]);
            layout->addRow(new QLabel(opt.desc), opt.val);
            values.insert(items[2], opt);
        }
        else if ( items[3] == "Arg.Int" ) {
            AppOption opt(items[1], items[2], AppOption::IntOpt, items[5]);
            layout->addRow(new QLabel(opt.desc), opt.val);
            values.insert(items[2], opt);
        }
        else { /* TO FIX */ }
    }
    file.close();

    container->setLayout(layout);
    QScrollArea *scroll = new QScrollArea(this);
    scroll->setWidget(container);
    tabs->addTab(scroll, title);
}

Options::~Options()
{
}

Options* Options::instance = NULL;

Options* Options::getInstance(void)
{
    if ( instance == NULL )
      instance = new Options();
    return instance;
}
