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

#include <QTextEdit>
#include "syntax_highlighter.h"

class AppFile {
public:
    bool readOnly;
    bool upToDate;
    QString path;
    QFileInfo info;
    QTextEdit* text;
    SyntaxHighlighter *syntax;
    AppFile(QString path, bool readOnly, bool upToDate, QTextEdit *edit, SyntaxHighlighter *sh)
      : readOnly(readOnly), upToDate(upToDate), path(path), info(path), text(edit), syntax(sh) { }
    ~AppFile() {
        if ( syntax != NULL ) delete syntax;
        if ( text != NULL ) delete text;
    }
};

#endif
