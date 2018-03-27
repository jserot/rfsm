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
