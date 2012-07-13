\docType{methods}
\name{x}
\alias{height}
\alias{height,Rectangle-method}
\alias{height<-}
\alias{height<-,Rectangle-method}
\alias{width}
\alias{width,Rectangle-method}
\alias{width<-}
\alias{width<-,Rectangle-method}
\alias{x}
\alias{x,Rectangle-method}
\alias{x<-}
\alias{x<-,Rectangle-method}
\alias{y}
\alias{y,Rectangle-method}
\alias{y<-}
\alias{y<-,Rectangle-method}
\title{Dimensions of the Rectangular object}
\value{
  \code{x(),y(),width(),height()}: a string value for
  single coordinates and dimensions methods

  \code{x(),y(),width(),height()<-}: the rectangular object
  as invisible
}
\description{
  These methods are accessors to the location and dimension
  of a Rectangular object.

  The method \code{y(object)} return the location on the
  \emph{Y-axis} of the rectangular object.

  The method \code{width(object)} return the width of the
  rectangular object.

  The method \code{height(object)} return the width of the
  rectangular object.
}
\details{
  The method \code{x(object)} return the location on the
  \emph{X-axis} of the rectangular object.

  The \code{x(object)<-value} method can be used to set the
  \emph{X-axis} location of a rectangular object.

  The \code{y(object)<-value} method can be used to set the
  \emph{Y-axis} location of a rectangular object.

  The \code{width(object)<-value} method can be used to set
  the width of an rectangular object.

  The \code{height(object)<-value} method can be used to
  set the height of a rectangular object.
}

