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
