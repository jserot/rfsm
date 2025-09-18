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

#pragma once

#include <QString>
#include <QStringList>
#include <QJsonObject>
#include <QJsonArray>

class Response
{
public:
    enum class Kind {
        Version,
        Compiled,
        Checked,
        Error,
        None
    };

    // Public static ctors
    static Response Version(const QString &v);
    static Response CompilationOk(const QStringList &files);
    static Response CompilationFailed(const QString &msg);
    static Response CheckingOk();
    static Response CheckingFailed(const QString &msg);
    static Response Error(const QString &err);
    static Response None();

    // Accessors
    Kind kind() const;
    QString version() const;
    bool success() const;
    QStringList files() const;
    QString message() const;
    QString error() const;

    QJsonObject toJson() const;
    static Response fromJson(const QJsonObject &obj);
    static Response fromString(const QString &s);

private:
    Kind m_kind;
    QString m_version;
    bool m_success = false;
    QStringList m_files;
    QString m_message;
    QString m_error;

    // Private constructors
    explicit Response(Kind kind);
    Response(Kind kind, const QString &err);
    Response(Kind kind, const QStringList &files);
};
