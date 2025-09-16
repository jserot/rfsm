#include "mainWindow.h"
#include "ui_mainWindow.h"
#include "compiler.h"

#include <QDebug>
#include <QMessageBox>

const QString MainWindow::serverPath = "/Users/jserot/Dev/ml/rfsm/_build/default/src/guests/std/bin/rfsmc.exe2"; // TO BE FIXED
const QString MainWindow::socketPath = "/tmp/rfsm_sock";

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    QString wDir = QCoreApplication::applicationFilePath();
    qDebug() << "wdir=" << wDir;
    compiler = new Compiler(this);
    connect(compiler, &Compiler::serverError, this, &MainWindow::serverError);
    compiler->startServer(serverPath,socketPath);

    connect(ui->sendButton, &QPushButton::clicked, this, &MainWindow::sendRequest);
    connect(ui->inputLine, &QLineEdit::editingFinished, this, &MainWindow::sendRequest);
}

MainWindow::~MainWindow()
{
  if ( compiler ) delete compiler;
  delete ui;
}

void MainWindow::sendRequest()
{
    QString text = ui->inputLine->text().trimmed();
    if (text.isEmpty()) return;

    ui->logView->append("→ " + text);

    QString  response = compiler->sendRequest(text);
    qDebug() << "mainWindow: got response:" << response;

    ui->logView->append("← " + response);
    ui->inputLine->clear();
}

void MainWindow::serverError(QString error)
{
  QMessageBox::critical(this, "Compiler", error); 
  exit(1);
}
