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

#include "fsm_syntax_highlighter.h"

FsmSyntaxHighlighter::FsmSyntaxHighlighter(QTextDocument *parent)
    : SyntaxHighlighter(parent)
{
    HighlightingRule rule;

    rule.format.setForeground(Qt::blue);
    //rule.format.setFontWeight(QFont::Bold);
    rule.pattern = QRegExp("\\btype\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bfsm\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bmodel\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\binput\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\boutput\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bshared\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\s->\\s"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bon\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bwhen\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bwith\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\|\\s"); highlightingRules.append(rule);
    rule.pattern = QRegExp("!\\s"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkGreen);
    rule.format.setFontWeight(QFont::Normal);
    //rule.pattern = QRegExp("#[a-z]+\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bin\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bout\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\binout\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bstates\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bvars\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\btrans\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bitrans\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bsporadic\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bperiodic\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bvalue_changes\\b"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::darkMagenta);
    rule.pattern = QRegExp("\\bevent\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bint\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\bbool\\b"); highlightingRules.append(rule);
    rule.pattern = QRegExp("\\barray\\b"); highlightingRules.append(rule);
    //rule.pattern = QRegExp("[A-Za-z]+\\s*:"); highlightingRules.append(rule);
    rule.format.setForeground(Qt::gray);
    rule.format.setFontItalic(true);
    rule.pattern = QRegExp("-- [^\n]*"); highlightingRules.append(rule);
}
