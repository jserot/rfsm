#pragma once

#include <QWidget>
#include <QLocalSocket>

class QProcess;
class QLocalSocket;
class QPushButton;
class QLineEdit;
class QLabel;

class Client : public QWidget
{
    Q_OBJECT

public:
    explicit Client(QWidget *parent = nullptr);
    ~Client();

private slots:
    void sendRequest();
    void readAnswer();
    void handleError(QLocalSocket::LocalSocketError socketError);

private:
    void connectToServer();

    QProcess *m_process = nullptr;
    QLocalSocket *m_socket = nullptr;
    QLineEdit *requestText;
    QLabel *answerText;

    static const QString socketPath;
};
