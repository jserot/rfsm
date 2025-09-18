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

#include "fragment.h"

#include <QString>
#include <QPair>
#include <QList>
#include <QJsonObject>
#include <QJsonArray>

QJsonObject Fragment::toJson() const
{
        QJsonObject json;
        json["inps"] = listToJsonArray(inps);
        json["outps"] = listToJsonArray(outps);
        json["vars"] = listToJsonArray(vars);
        json["obj"] = obj;
        return json;
}

Fragment Fragment::fromJson(const QJsonObject &json)
{
        return Fragment(
            jsonArrayToList(json["inps"].toArray()),
            jsonArrayToList(json["outps"].toArray()),
            jsonArrayToList(json["vars"].toArray()),
            json["obj"].toString()
        );
}

QJsonArray Fragment::listToJsonArray(const QList<QPair<QString, QString>> &list)
{
        QJsonArray arr;
        for (const auto &p : list) {
            QJsonObject o;
            o["id"] = p.first;
            o["type"] = p.second;
            arr.append(o);
        }
        return arr;
}

QList<QPair<QString, QString>> Fragment::jsonArrayToList(const QJsonArray &arr)
{
        QList<QPair<QString, QString>> list;
        for (const auto &val : arr) {
            QJsonObject o = val.toObject();
            list.append(qMakePair(o["id"].toString(), o["type"].toString()));
        }
        return list;
}
