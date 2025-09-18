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

#include "request.h"

Request Request::GetVersion() { return Request(Kind::GetVersion); }
Request Request::CheckFragment(const Fragment &frag) { return Request(frag); }
Request Request::Compile(const QStringList &args) { return Request(args); }
Request Request::Close() { return Request(Kind::Close); }

Request::Kind Request::kind() const { return m_kind; }
Fragment Request::fragment() const { return m_fragment; }
QStringList Request::files() const { return m_files; }

QJsonObject Request::toJson() const {
    QJsonObject obj;
    switch (m_kind) {
    case Kind::GetVersion:
        obj["kind"] = "version";
        break;
    case Kind::Close:
        obj["kind"] = "close";
        break;
    case Kind::CheckFragment:
        obj["kind"] = "check";
        obj["fragment"] = m_fragment.toJson();
        break;
    case Kind::Compile: {
        obj["kind"] = "compile";
        QJsonArray arr;
        for (const auto &s : m_files) {
            arr.append(s);
        }
        obj["args"] = arr;
        break;
    }
    }
    return obj;
}

Request Request::fromJson(const QJsonObject &obj) {
    QString kind = obj["kind"].toString();
    if (kind == "GetVersion") {
        return GetVersion();
    } else if (kind == "Close") {
        return Close();
    } else if (kind == "CheckFragment") {
        Fragment f = Fragment::fromJson(obj["fragment"].toObject());
        return CheckFragment(f);
    } else if (kind == "Compile") {
        QStringList files;
        for (const auto &v : obj["files"].toArray()) {
            files.append(v.toString());
        }
        return Compile(files);
    }
    return Close(); // default fallback
}

Request Request::fromString(const QString &jsonStr)
{
    QJsonParseError err;
    QJsonDocument doc = QJsonDocument::fromJson(jsonStr.toUtf8(), &err);
    if (err.error != QJsonParseError::NoError) {
        return Request::Close(); // default fallback
    }
    return fromJson(doc.object());
}

Request::Request(Kind kind) : m_kind(kind) {}
Request::Request(const Fragment &frag) : m_kind(Kind::CheckFragment), m_fragment(frag) {}
Request::Request(const QStringList &files) : m_kind(Kind::Compile), m_files(files) {}
