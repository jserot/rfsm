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

#ifndef _app_options_h
#define _app_options_h

#include <QCheckBox>
#include <QLineEdit>

class AppOption 
{
 public:
  typedef enum { UnitOpt, StringOpt, IntOpt } Opt_kind;
  QString category;
  QString name;
  Opt_kind kind;
  QString desc;
  QCheckBox *checkbox;
  QLineEdit *val;
  AppOption() : category(""), name("") { }
  AppOption(QString _category, QString _name, Opt_kind _kind, QString _desc);
  ~AppOption() { }
};
#endif
