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

#include "response.h"

// Public ctors
Response Response::Version(const QString &v) { return Response(Kind::Version,v); }
Response Response::Error(const QString &msg) { return Response(Kind::Error, msg); }
Response Response::CompilationFailed(const QString &msg) { return Response(Kind::Compiled, msg); }
Response Response::CheckingFailed(const QString &msg) { return Response(Kind::Checked, msg); }
Response Response::CompilationOk(const QStringList &files) { return Response(Kind::Compiled, files); }
Response Response::CheckingOk() { return Response(Kind::Checked); }
Response Response::None() { return Response(Kind::None); }

// Private ctors
Response::Response(Kind kind) : m_kind(kind) // None, CheckingOk
{
  switch ( kind ) {
  case Kind::Checked: m_success = true; break;
  default: break;
  }
}

Response::Response(Kind kind, const QString &s) : m_kind(kind) // Version, CompilationFailed, CheckingFailed, Error
{
  switch ( kind ) {
  case Kind::Version: m_version = s; break;
  case Kind::Compiled: m_success = false; m_message = s; break;
  case Kind::Checked: m_success = false; m_message = s; break;
  case Kind::Error: m_message = s; break;
  default: break; // Should not happen
  }
}

Response::Response(Kind kind, const QStringList &ss)
  : m_kind(kind), m_success(true), m_files(ss) {} // CompilationOk

Response::Kind Response::kind() const { return m_kind; }
QString Response::version() const { return m_version; }
bool Response::success() const { return m_success; }
QStringList Response::files() const { return m_files; }
QString Response::message() const { return m_message; }
QString Response::error() const { return m_error; }

QJsonObject Response::toJson() const {
    QJsonObject obj;
    switch (m_kind) {
    case Kind::Version:
        obj["kind"] = "version";
        obj["version"] = m_version;
        break;
    case Kind::Compiled: {
        obj["kind"] = "compiled";
        obj["success"] = m_success;
        QJsonArray arr;
        for (const auto &f : m_files) arr.append(f);
        obj["files"] = arr;
        break;
    }
    case Kind::Checked:
        obj["kind"] = "Checked";
        obj["success"] = m_success;
        obj["message"] = m_message;
        break;
    case Kind::Error:
        obj["kind"] = "Error";
        obj["error"] = m_error;
        break;
    case Kind::None:
        obj["kind"] = "None";
        break;
    }
    return obj;
}

Response Response::fromJson(const QJsonObject &obj) {
    QString kind = obj["kind"].toString();
    if (kind == "version") {
        return Version(obj["version"].toString());
    } else if (kind == "compiled") {
        bool success = obj["result"].toBool();
        if ( success ) {
          QStringList files;
          for (const auto &v : obj["files"].toArray()) files.append(v.toString());
          return CompilationOk(files);
          }
        else {
          QString msg = obj["message"].toString();
          return CompilationFailed(msg);
          }
    } else if (kind == "checked") {
        bool success = obj["success"].toBool();
        if ( success ) 
          return CheckingOk();
        else {
          QString msg = obj["message"].toString();
          return CheckingFailed(msg);
          }
    } else if (kind == "error") {
        return Error(obj["error"].toString());
    } else {
        return None();
    }
}

Response Response::fromString(const QString &jsonStr)
{
    QJsonParseError err;
    QJsonDocument doc = QJsonDocument::fromJson(jsonStr.toUtf8(), &err);
    if (err.error != QJsonParseError::NoError) {
        return Response::None(); // default fallback
    }
    return fromJson(doc.object());
}

