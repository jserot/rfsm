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

#ifndef ImageViewer_H
#define ImageViewer_H

#include <QScrollArea>

QT_BEGIN_NAMESPACE
class QImage;
class QLabel;
QT_END_NAMESPACE

class ImageViewer : public QScrollArea
{
  Q_OBJECT

public:
  ImageViewer(QWidget *parent = 0);

  void scaleImage(double scaleFactor);
  void setPixmap(const QPixmap& pixmap);
  void adjustImageSize(void);

  bool isImageLoaded(void);
  bool isFittedToWindow(void);
  double getScaleFactor(void);

public slots:
  void fitToWindow(const bool& bValue);

private slots:
    void normalSize();

private:
    static const double minScaleFactor;
    static const double maxScaleFactor;
    double currentScaleFactor;
    bool imageIsLoaded;
    bool fittedToWindow;
    QLabel *imageLabel;

    void adjustScrollBar(QScrollBar *scrollBar, double factor);
};

#endif
