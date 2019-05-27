/**********************************************************************/
/*                                                                    */
/*              This file is part of the RFSM package                 */
/*                                                                    */
/*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  */
/*                                                                    */
/*  This source code is licensed under the license found in the       */
/*  LICENSE file in the root directory of this source tree.           */
/*                                                                    */
/**********************************************************************/

#ifndef _app_file_h
#define _app_file_h

#include <QPlainTextEdit>
#include "syntax_highlighter.h"

class AppFile {
public:
    bool ronly;
    bool upToDate;
    QFileInfo info;
    QPlainTextEdit* text;
    SyntaxHighlighter *syntax;
    AppFile(QString path, bool ronly, QPlainTextEdit *edit, SyntaxHighlighter *sh)
      : ronly(ronly), upToDate(true), info(path), text(edit), syntax(sh) { }
    AppFile()
      : ronly(true), upToDate(true), info(""), text(NULL), syntax(NULL) { }
    ~AppFile() {
        if ( syntax != NULL ) delete syntax;
        if ( text != NULL ) delete text;
    }
};

#endif
