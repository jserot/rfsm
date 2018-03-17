#ifndef _app_options_h
#define _app_options_h

#include <QCheckBox>
#include <QLineEdit>

class AppOption 
{
 public:
  typedef enum { UnitOpt, StringOpt, IntOpt } Opt_type;
  QString category;
  QString name;
  Opt_type typ;
  QString desc;
  QCheckBox *checkbox;
  QLineEdit *val;
  AppOption() : category(""), name("") { }
  AppOption(QString _category, QString _name, Opt_type _typ, QString _desc);
  ~AppOption() { }
};
#endif
