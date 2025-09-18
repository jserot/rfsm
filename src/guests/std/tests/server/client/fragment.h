/***********************************************************************/
/*                                                                     */
/*       This file is part of the Grasp software package               */
/*                                                                     */
/*  Copyright (c) 2019-present, Jocelyn SEROT (jocelyn.serot@uca.fr)   */
/*                       All rights reserved.                          */
/*                                                                     */
/*    This source code is licensed under the license found in the      */
/*      LICENSE file in the root directory of this source tree.        */
/*                                                                     */
/***********************************************************************/

// Model fragments (state valuations, guards, actions) sent to the compiler server for checking

#pragma once

#include <QString>
#include <QPair>
#include <QList>
#include <QJsonObject>
#include <QJsonArray>

class Fragment
{
private:
    QList<QPair<QString, QString>> inps;
    QList<QPair<QString, QString>> outps;
    QList<QPair<QString, QString>> vars;
    QString obj;

public:
    Fragment() = default;

    Fragment(const QList<QPair<QString, QString>> &inps,
      const QList<QPair<QString, QString>> &outps,
      const QList<QPair<QString, QString>> &vars,
      const QString &obj)
        : inps(inps), outps(outps), vars(vars), obj(obj) {}

  QJsonObject toJson() const;

  static Fragment fromJson(const QJsonObject &json);

private:
  static QJsonArray listToJsonArray(const QList<QPair<QString, QString>> &list);
  static QList<QPair<QString, QString>> jsonArrayToList(const QJsonArray &arr);
};
