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

