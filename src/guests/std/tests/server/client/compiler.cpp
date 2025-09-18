#include "compiler.h"
#include <QTimer>
#include <QJsonDocument>
#include <QJsonObject>
#include <QDebug>

Compiler::Compiler(QObject *parent) : QObject(parent)
{
    serverProcess.setProcessChannelMode(QProcess::ForwardedChannels);

    connect(&socket, &QLocalSocket::connected, this, &Compiler::onConnected);
    // connect(&socket, &QLocalSocket::readyRead, this, &Compiler::onReadyRead);  // not used here
    connect(&socket, &QLocalSocket::disconnected, this, &Compiler::onDisconnected);
    connect(&socket, &QLocalSocket::errorOccurred, this, &Compiler::onErrorOccurred);
}

Compiler::~Compiler() {
  qDebug() << "compiler: done";
  stopServer();
}

void Compiler::startServer(const QString &serverPath, const QString &socketPath)
{
    this->socketPath = socketPath;
    QStringList serverArgs;
    serverArgs << "-server_mode" << "-socket_path" << socketPath;
    qDebug() << "compiler: launching:" << serverPath << serverArgs;
    serverProcess.start(serverPath, serverArgs);
    if ( serverProcess.waitForStarted(3000) ) {
      qDebug() << "compiler: server started in" << serverProcess.workingDirectory();
      //emit serverStarted();
      QTimer::singleShot(300, this, [this]() { socket.connectToServer(this->socketPath); });
      }
    else {
      qDebug() << "compiler: cannot launch server";
      emit serverError("Cannot launch compiler server");
      }
}

QString Compiler::sendRequest(const QString &text)
{
    if (socket.state() == QLocalSocket::ConnectedState) {
        QByteArray data = text.trimmed().toUtf8() + '\n';
        //qDebug() << "compiler: sending: " << data;
        socket.write(data);
        socket.flush();
        //qDebug() << "compiler: sent";
        QString response = readAnswer();
        return response;
        }
    else {
        qDebug() << "compiler: server error: no active connexion";
        return "";
        }
}

QString Compiler::readAnswer()
{
  if (socket.waitForReadyRead(3000)) {
    QByteArray line = socket.readAll();
    line.chop(1);
    return line;
    }
  else
    qDebug() << "compiler: timeout when waiting for response";
    return "<no response>";
}

void Compiler::stopServer() {
    if (socket.state() == QLocalSocket::ConnectedState) {
        socket.disconnectFromServer();
    }
    if (serverProcess.state() == QProcess::Running) {
        serverProcess.terminate();
        serverProcess.waitForFinished();
        qDebug() << "compiler: server terminated";
    }
}

void Compiler::onConnected() {
  qDebug() << "compiler: connected to server";
    //emit connected();
}

// void Compiler::onReadyRead() {
//     QByteArray raw = socket.readAll();
//     raw.chop(1);
//     QString response = raw;
//     qDebug() << "compiler: got response:" << response;
//     //emit messageReceived(response):
// }

void Compiler::onDisconnected() {
    // emit disconnected();
  qDebug() << "compiler: disconnected from server";
}

void Compiler::onErrorOccurred(QLocalSocket::LocalSocketError) {
    qDebug() << "compiler: got error:" << socket.errorString();
    //emit serverError(socket.errorString());
}
