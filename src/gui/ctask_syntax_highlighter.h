#ifndef _CTASK_HIGHLIGHTER_H
#define _CTASK_HIGHLIGHTER_H

#include "syntax_highlighter.h"

class CTaskSyntaxHighlighter : public SyntaxHighlighter
{
public:
  CTaskSyntaxHighlighter(QTextDocument *parent = 0);
};

#endif
