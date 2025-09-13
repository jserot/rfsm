#include "client.h"

#include <QDebug>
#include <QProcess>
#include <QTimer>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QGridLayout>
#include <QApplication>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMessageBox>

#include <QLocalSocket>

const QString Client::socketPath = "/tmp/rfsm_sock";

Client::Client(QWidget *parent) : QWidget(parent)
{
    m_socket = new QLocalSocket(this);

    // Start server
    m_process = new QProcess(this);
    connect(m_process, &QProcess::errorOccurred, this, [this](QProcess::ProcessError error) {
      Q_UNUSED(this);
      qWarning() << "Error when launching/running server process:" << error;
    });

    m_process->setProcessChannelMode(QProcess::ForwardedChannels);

    //const QString clientDir = QApplication::applicationDirPath();
    const QString serverPath = "/Users/jserot/Dev/ml/rfsm/_build/default/src/guests/std/bin/rfsmc.exe"; // TO BE FIXED
    QStringList serverArgs;
    serverArgs << "-server_mode" << "-socket_path" << socketPath;
    qDebug() << "Client::serverPath=" << serverPath;
    qDebug() << "Client::server args=" << serverArgs;
    m_process->start(serverPath, serverArgs);
    const bool ok = m_process->waitForStarted(); // so we can connect to it
    if (ok) {
      qDebug() << "Client: server started";
      } // otherwise errorOccured() is emitted

    // Build GUI

    QLabel *requestLabel = new QLabel("Fragment:");
    requestText = new QLineEdit();
    requestLabel->setBuddy(requestText);
    answerText = new QLabel("<response displayed here>");

    connect(requestText, &QLineEdit::editingFinished, this, &Client::sendRequest);

    QGridLayout *mainLayout = new QGridLayout(this);
    mainLayout->addWidget(requestLabel, 0, 0);
    mainLayout->addWidget(requestText, 0, 1);
    mainLayout->addWidget(answerText, 1, 0, 1, 2);

    setWindowTitle("Client");
    //requestText->setFocus();

    // Prepare socket
    connect(m_socket, &QLocalSocket::connected, this, [this]() { requestText->setEnabled(true); });
    connect(m_socket, &QLocalSocket::disconnected, this, [this]() { requestText->setEnabled(false); });
    connect(m_socket, &QLocalSocket::readyRead, this, &Client::readAnswer); 
    connect(m_socket, &QLocalSocket::errorOccurred, this, &Client::handleError);

    // Go!
    connectToServer();
}

Client::~Client()
{
    qDebug() << "Client: bye !";
    if (m_socket->state() == QLocalSocket::ConnectedState) {
        if (m_process && m_process->state() == QProcess::Running) {
          //m_socket->write("{\"close\":\"\"}\n");
          //m_socket->flush();
          // qDebug() << "Client: sent close request !";
            m_process->waitForFinished();
            qDebug() << "Client: server process finished !";
        }
        m_socket->disconnectFromServer();
        qDebug() << "Client: disconnected from server!";
    }
}

void Client::connectToServer()
{
  qDebug() << "Client connecting on" << socketPath;
  m_socket->connectToServer(socketPath); // note that this returns immediately
}

void Client::sendRequest()
{
  QByteArray line = requestText->text().trimmed().toUtf8() + '\n';
  qDebug() << "Client: sending line: " << line;
  m_socket->write(line);
  m_socket->flush();
  readAnswer();
}

void Client::readAnswer()
{
    while (m_socket->canReadLine()) {
        QByteArray line = m_socket->readLine();
        line.chop(1);
        qDebug() << "Client: got response:" << line;
        // QJsonDocument doc = QJsonDocument::fromJson(line);
        // QJsonObject obj = doc.object();
        // QString res =
        //   obj.keys().contains("version") ?
        //   obj["version"].toString() :
        //   obj["result"].toString();
        // qDebug() << "Client: response=" << res;
        answerText->setText(line);
        }
}

void Client::handleError(QLocalSocket::LocalSocketError socketError)
{
        // qDebug() << "Client: error:" << socketError << "retrying in 100ms";
        // Q_UNUSED(socketError);
        // requestText->setEnabled(false);
        // QTimer::singleShot(500, this, [this]() { connectToServer(); });
    switch (socketError) {
    case QLocalSocket::ServerNotFoundError:
        QMessageBox::information(this, tr("Client"),
                                 tr("The host was not found. Please make sure "
                                    "that the server is running and that the "
                                    "server name is correct."));
        break;
    case QLocalSocket::ConnectionRefusedError:
        QMessageBox::information(this, tr("Client"),
                                 tr("The connection was refused by the server. "));
        break;
    case QLocalSocket::PeerClosedError:
        break;
    default:
        QMessageBox::information(this, tr("Client"),
                                 tr("The following error occurred: %1.").arg(m_socket->errorString()));
    }
}
