#ifndef FSFILTER_H
#define FSFILTER_H

#include <QSortFilterProxyModel>

class FileFilter : public QSortFilterProxyModel
{
public:
 FileFilter(QObject *parent, QStringList& excluded)
    : QSortFilterProxyModel(parent), excluded(excluded) {}
protected:
    QStringList excluded;
    bool filterAcceptsRow(int row, const QModelIndex &par) const;
    /* bool lessThan(const QModelIndex &left, const QModelIndex &right) const; */
};

#endif
