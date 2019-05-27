#include "file_Filter.h"
#include <QFileSystemModel>
#include <qDebug>

bool FileFilter::filterAcceptsRow(int row, const QModelIndex &par) const
{
  QModelIndex idx = sourceModel()->index(row, 0, par);
  QString fname = idx.data(QFileSystemModel::FileNameRole).toString();
  for ( int i = 0; i < excluded.size(); ++i ) {
    QRegExp rx(excluded.at(i));
    rx.setPatternSyntax(QRegExp::Wildcard);
    if ( rx.exactMatch(fname) ) return false;
    }
  return true;
}

// bool FileFilter::lessThan(const QModelIndex &left, const QModelIndex &right) const
// {
//   QFileSystemModel* model = qobject_cast<QFileSystemModel*>(sourceModel());
//   QString l = left.data(QFileSystemModel::FileNameRole).toString();
//   QString r = right.data(QFileSystemModel::FileNameRole).toString();
//   qDebug() << l << ":" << r;
//   if ( !model->fileInfo(left).isDir() && model->fileInfo(right).isDir() ) {
//      return false;
//      }
//   return true;
// }
