#ifndef _COMPILER_H
#define _COMPILER_H

#include <QObject>
#include <QLocalSocket>
#include <QProcess>
#include <QJsonValue>

class Compiler : public QObject {
    Q_OBJECT

public:
    explicit Compiler(QObject *parent = nullptr);
    ~Compiler();

    void startServer(const QString &serverPath, const QString &socketPath);
    QString sendRequest(const QString &text);
    void stopServer();

signals:
    void serverStarted();
    void serverError(const QString &error);
    // void connected();
    void messageReceived(const QString &message);
    // void disconnected();

private slots:
    void onConnected();
    // void onReadyRead();  // For asynchronous reception of responses; not used here
    void onDisconnected();
    void onErrorOccurred(QLocalSocket::LocalSocketError socketError);

private:
    static const int TimeOutMs = 2000;  // Timeout when waiting for a response after sending a request (synchronous mode)

    QString readAnswer();
    QProcess serverProcess;
    QLocalSocket socket;
    QString socketPath;
};

#endif // _COMPILER_H
