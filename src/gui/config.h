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

#ifndef _config_h
#define _config_h

#include <QDialog>
#include "ui_config.h"

namespace Ui {
    class config;
}

class config : public QDialog, public Ui_config
{
    Q_OBJECT

public:

    static config* getInstance(void);

    QString getPath(QString name);
    void setPath(QString name, QString val);

private slots:

    void setCompilerPath();
    void setDotProgramPath();
    void setDotViewerPath();
    void setVcdViewerPath();
    void setTxtViewerPath();

private:

    Ui::config *ui;

    explicit config(QWidget *parent = 0);

    ~config();

    static config* instance;
};

#endif // CONFIG_H
