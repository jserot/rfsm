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

#ifndef _project_h
#define _project_h

#include <QString>
#include <QStringList>

class Project {
public:
  QString file;
  QString dir;
  QString mainName;
  QStringList srcFiles;
  QString dotOptions;
  QString simOptions;
  QString ctaskOptions;
  QString systemcOptions;
  QString vhdlOptions;
  Project()
    : file(), dir(), mainName(), srcFiles(), dotOptions(), simOptions(), ctaskOptions(), systemcOptions(), vhdlOptions() { }
  Project(QString fname) { readFromFile(fname); };
  ~Project() { }
  void save();
  bool readFromFile(QString fname);
  bool writeToFile(QString fname);
  void addSrcFile(QString fname);
public:
  static QString fileSuffix;
};

#endif
