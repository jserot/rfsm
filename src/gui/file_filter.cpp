#include "file_Filter.h"
#include <QFileSystemModel>

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
// }
