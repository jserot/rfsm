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

// A class for keeping track of compiler options

#ifndef _options_h
#define _options_h

#include <QDialog>
#include <QFile>
#include <QMap>
#include <QTabWidget>
#include <QFormLayout>
#include <QLabel>
#include <QCheckBox>
#include <QTabWidget>
#include <QScrollArea>
#include <QDialogButtonBox>
#include "option.h"
#include "project.h"

class Options {
public:
    Options(QString specFile, QWidget *parent);
    ~Options();

    QMap<QString,AppOption> opts;

    void show(QWidget *parent, QString title);
    QString readFromProject(Project *project);

    QString toString(QString cat);

private:

    QString parse_opts(QString cat, QString line);
    void addTab(QString title, QString category, QTabWidget *tabs, QWidget *parent);
};

#endif
