#ifndef _COMMANDLINE_H
#define _COMMANDLINE_H

#include <QString>

class CommandLine
{
public:
  QString cmd;
  QString args;

  CommandLine(QString cmd_, QString args_ = "") : cmd(cmd_), args(args_) { }
  CommandLine() : cmd(""), args("") { }
  ~CommandLine() { }

  QString toString(void);
};

#endif
