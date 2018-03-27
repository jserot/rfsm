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

#include "ctask_syntax_highlighter.h"

CTaskSyntaxHighlighter::CTaskSyntaxHighlighter(QTextDocument *parent)
    : SyntaxHighlighter(parent)
{
    HighlightingRule rule;

    rule.format.setForeground(Qt::blue);
    rule.format.setFontWeight(QFont::Bold);
    rule.pattern = QRegExp("\\btask\\b"); highlightingRules.append(rule);
    rule.format.setFontWeight(QFont::Normal);
    rule.pattern = QRegExp("\\bin\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bout\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\inout\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkYellow);
    rule.pattern = QRegExp("\\wait_ev\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\wait_evs\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\notify_ev\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkGreen);
    rule.pattern = QRegExp("#[a-z]+\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bwhile\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bswitch\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bcase\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bbreak\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bif\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\belse\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkMagenta);
    rule.pattern = QRegExp("\\bevent\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bint\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bbool\\b"); highlightingRules.append(rule);
    //rule.pattern = QRegExp("[A-Za-z]+\\s*:"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::gray);
    rule.format.setFontItalic(true);
    rule.pattern = QRegExp("//[^\n]*"); highlightingRules.append(rule);
}
