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

#include <QtWidgets>
#include "ImageViewer.h"

#include <QtGui>
#include <QPainter>

const double ImageViewer::minScaleFactor = 0.1;
const double ImageViewer::maxScaleFactor = 2.0;

ImageViewer::ImageViewer(QWidget *parent) : QScrollArea(parent)
{
  imageLabel = new QLabel;
  imageLabel->setBackgroundRole(QPalette::Base);
  imageLabel->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
  imageLabel->setScaledContents(true);
  this->setBackgroundRole(QPalette::Dark);
  this->setWidget(imageLabel);
  imageIsLoaded = false;
  fittedToWindow = false;
  currentScaleFactor = 1.0;
}

void ImageViewer::scaleImage(double scaleFactor)
{
  if ( scaleFactor > maxScaleFactor || scaleFactor < minScaleFactor) return;
  Q_ASSERT(imageLabel->pixmap());
  imageLabel->resize( scaleFactor * imageLabel->pixmap()->size());
  adjustScrollBar(this->horizontalScrollBar(), scaleFactor);
  adjustScrollBar(this->verticalScrollBar(), scaleFactor);
  update();
  updateGeometry();
  currentScaleFactor = scaleFactor;
}

bool ImageViewer::isImageLoaded()
{
    return imageIsLoaded;
}

bool ImageViewer::isFittedToWindow()
{
    return fittedToWindow;
}

double ImageViewer::getScaleFactor()
{
    return currentScaleFactor;
}

void ImageViewer::setPixmap(const QPixmap &pixmap)
{
    imageLabel->setPixmap(pixmap);
    imageIsLoaded = true;
    currentScaleFactor = 1.0;
}

void ImageViewer::adjustImageSize()
{
    imageLabel->adjustSize();
}

void ImageViewer::normalSize()
{
  imageLabel->adjustSize();
  currentScaleFactor = 1.0;
}

void ImageViewer::fitToWindow(const bool &t)
{
  this->setWidgetResizable(t);
  if ( !t ) normalSize();
  fittedToWindow = t;
}

void ImageViewer::adjustScrollBar(QScrollBar *scrollBar, double factor)
{
  scrollBar->setValue(int(factor * scrollBar->value() + ((factor - 1) * scrollBar->pageStep()/2)));
}
