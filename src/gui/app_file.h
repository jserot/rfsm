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
    bool upToDate;
    QString path;
    QString name;
    QTextEdit* text;
    SyntaxHighlighter *syntax;
    AppFile(QString path_, bool upToDate_, QTextEdit *edit, SyntaxHighlighter *sh)
        : upToDate(upToDate_), path(path_), text(edit), syntax(sh) {
        QFileInfo f(path);
        name = f.fileName();
    }
    ~AppFile() {
        if ( syntax != NULL ) delete syntax;
        if ( text != NULL ) delete text;
    }
};

#endif
