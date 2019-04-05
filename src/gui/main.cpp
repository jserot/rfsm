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

#include <QApplication>
#include "mainwindow.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QCoreApplication::setApplicationVersion(QT_VERSION_STR);
    QCommandLineParser parser;
    parser.setApplicationDescription("RFSM IDE");
    parser.addHelpOption();
    parser.addVersionOption();
    // QCommandLineOption showFileInfoOption("f", "Display file info");
    // parser.addOption(showFileInfoOption);
    parser.addPositionalArgument("directory", "The directory to start in.");
    parser.process(app);
    QString initDir = parser.positionalArguments().isEmpty() ? "" : parser.positionalArguments().at(0);

    MainWindow w(initDir);
    w.show();

    return app.exec();
}
