#include "project.h"

#include <QFile>
#include <QFileInfo>
#include <QTextStream>
#include <QDebug>

QString Project::fileSuffix = "pro";

bool Project::readFromFile(QString fname)
{
  QFile f(fname);
  if ( ! f.exists() ) return false;
  if ( ! f.open(QIODevice::ReadOnly | QIODevice::Text) ) return false;
  QFileInfo fi(fname);
  file = fi.canonicalFilePath();
  dir = fi.absolutePath();
  qDebug() << "Reading project file " << file << " in " << dir;
  QTextStream s(&f);
  while( ! s.atEnd() ) {
      QStringList l = s.readLine().split("=");
      //qDebug() << "  Project file " << fname << ": l" << "=" << l;
      if ( l.length() == 2 && l[0] != "" && l[1] != "" ) {
          QString key = l[0].trimmed();
          QString val = l[1].trimmed();
          qDebug() << "  key=" << key << " val=" << val ;
          if ( key == "MAIN" ) mainName = val;
          if ( key == "SRCS" ) srcFiles = val.split(" ");
          if ( key == "DOT_OPTS" ) dotOptions = val;
          if ( key == "SIM_OPTS" ) simOptions = val;
          if ( key == "CTASK_OPTS" ) ctaskOptions = val;
          if ( key == "SYSTEMC_OPTS" ) systemcOptions = val;
          if ( key == "VHDL_OPTS" ) vhdlOptions = val;
        }
    }
  s.flush();
  f.close();
  return true;
}

bool Project::writeToFile(QString fname)
{
  QFileInfo fi(fname);
  QFile f(fname);
  if ( ! f.open(QFile::WriteOnly | QFile::Text) ) return false;
  qDebug() << "Saving project as file " << fi.canonicalFilePath();
  QTextStream os(&f);
  os << "MAIN=" << mainName << endl;
  os << "SRCS=" << srcFiles.join(" ") << endl;
  os << "DOT_OPTIONS=" << dotOptions << endl;
  os << "SIM_OPTIONS=" << simOptions << endl;
  os << "CTASK_OPTIONS=" << ctaskOptions << endl;
  os << "SYSTEMC_OPTIONS=" << systemcOptions << endl;
  os << "VHDL_OPTIONS=" << vhdlOptions << endl;
  os.flush();
  f.close();
  file = fi.canonicalFilePath();
  dir = fi.absolutePath();
  return true;
}

void Project::save()
{
  if ( ! file.isEmpty() ) writeToFile(file);
}

void Project::addSrcFile(QString fname)
{
  if ( ! srcFiles.contains(fname) ) srcFiles.append(fname);
}
