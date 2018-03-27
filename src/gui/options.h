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

class Options: public QDialog {
    Q_OBJECT

public:
    static Options* getInstance(void);
    QMap<QString,AppOption> values;
    QSize sizeHint() const { return QSize(700, 300); }

private:
        QTabWidget* tabs;
        QDialogButtonBox *buttonBox;

        static Options* instance;
        explicit Options(QWidget *parent = 0);

        ~Options();

  void addTab(QString title, QString category);
};

#endif
