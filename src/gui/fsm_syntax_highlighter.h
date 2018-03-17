#ifndef _FSM_HIGHLIGHTER_H
#define _FSM_HIGHLIGHTER_H

#include "syntax_highlighter.h"

class FsmSyntaxHighlighter : public SyntaxHighlighter
{
public:
  FsmSyntaxHighlighter(QTextDocument *parent = 0);
};

#endif
