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

#ifndef _CTASK_HIGHLIGHTER_H
#define _CTASK_HIGHLIGHTER_H

#include "syntax_highlighter.h"

class CTaskSyntaxHighlighter : public SyntaxHighlighter
{
public:
  CTaskSyntaxHighlighter(QTextDocument *parent = 0);
};

#endif
