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
