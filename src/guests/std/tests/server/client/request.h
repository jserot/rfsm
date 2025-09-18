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

// Compiler server requests 

#pragma once

#include <QString>
#include <QStringList>
#include <QJsonObject>
#include <QJsonArray>
#include "fragment.h"

class Request
{
public:
    enum class Kind {
        GetVersion,
        CheckFragment,
        Compile,
        Close
    };

    static Request GetVersion();
    static Request Close();
    static Request CheckFragment(const Fragment &frag);
    static Request Compile(const QStringList &args);

    Kind kind() const;
    Fragment fragment() const;
    QStringList files() const;

    QJsonObject toJson() const;
    static Request fromJson(const QJsonObject &obj);
    static Request fromString(const QString &jsonStr);

private:
    Kind m_kind;
    Fragment m_fragment;
    QStringList m_files;

    explicit Request(Kind kind);
    explicit Request(const Fragment &frag);
    explicit Request(const QStringList &files);
};
