#include <QCoreApplication>
#include <QDebug>
#include <QTimer>
#include "fragment.h"
#include "request.h"
#include "response.h"
#include "compiler.h"

const QString serverPath = "/Users/jserot/Dev/ml/rfsm/_build/default/src/guests/std/bin/rfsmc.exe"; // TO BE ADJUSTED
const QString socketPath = "/tmp/rfsm_sock";

QString request_to_string(Request &r)
{
    QJsonObject json = r.toJson();
    QJsonDocument doc(json);
    QByteArray data = doc.toJson(QJsonDocument::Compact);
    return QString::fromUtf8(data);
}

static Compiler *compiler;

Request requests[] = {
  Request::GetVersion(),
  Request::Compile({"-ctask", "/Users/jserot/Dev/ml/rfsm/src/guests/std/tests/server/client/sample.fsm"}),
  //Request::Compile({"-ctask", "non_existent_file.fsm"}),
  Request::CheckFragment(Fragment({{"h","event"},{"e","int"}}, {{"o","bool"}}, {{"k","int"}}, "sval o=1")),
  Request::CheckFragment(Fragment({{"h","event"},{"e","int"}}, {{"o","bool"}}, {{"k","int"}}, "sval o=2")),
  Request::Close()
};

constexpr int requestCount = sizeof(requests) / sizeof(requests[0]);

void sendRequest(void)
{
  static int i = 0;
  Request q = requests[i];
  qDebug() << "** client: sending request:" << q.toJson();
  QString s = request_to_string(q); 
  //qDebug() << "client: encoded request:" << s;
  s = compiler->sendRequest(s);
  //qDebug() << "client: got response:" << s;
  Response r = Response::fromString(s);
  qDebug() << "** client: got response:" << r.toJson();
  if ( q.kind() == Request::Kind::Close ) {
    qDebug() << "** client: bye !";
    exit(1);
    }
  i = (i+1) % requestCount;
}

int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    QTimer *timer = new QTimer(&app);
    QObject::connect(timer, &QTimer::timeout, &sendRequest);

    QString wDir = QCoreApplication::applicationFilePath();
    qDebug() << "wdir=" << wDir;
    compiler = new Compiler();
    compiler->startServer(serverPath,socketPath);
    timer->start(1000);

    return app.exec(); // Boucle d’événements

    return 0;
}
