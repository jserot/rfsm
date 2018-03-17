#include "command.h"

QString CommandLine::toString()
{
#ifdef Q_OS_WIN
  QString cmdPath = cmd.contains(' ') ? "\"" + cmd + "\"" : cmd;
#else
  QString cmdPath = cmd;
#endif
  return cmdPath + " " + args;
}

