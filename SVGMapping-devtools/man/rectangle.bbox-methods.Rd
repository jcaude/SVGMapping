\docType{methods}
\name{bbox}
\alias{bbox}
\alias{bbox,Rectangle-method}
\alias{bbox<-}
\alias{bbox<-,Rectangle-method}
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
\arguments{
  \item{object}{a rectangular object}
}
\value{
  \code{bbox()}: a named list
  (\code{list(x=,y=,width=,height=)}) of values for the
  complete bounding-box given as strings.

  \code{bbox()<-}: the rectangular object as invisible

  \code{x(),y(),width(),height()}: a string value for
  single coordinates and dimensions methods

  \code{x(),y(),width(),height()<-}: the rectangular object
  as invisible
}
\description{
  These methods are accessors to the location and dimension
  of a Rectangular object.

  The method \code{x(object)} return the location on the
  \emph{X-axis} of the rectangular object.

  The method \code{y(object)} return the location on the
  \emph{Y-axis} of the rectangular object.

  The method \code{width(object)} return the width of the
  rectangular object.

  The method \code{height(object)} return the width of the
  rectangular object.
}
\details{
  The method \code{bbox(object)} return the bounding box
  (x,y,width,height) of the rectangular object.

  Coordinates or dimensions are given as values compliant
  with the SVG 1.1 specifications

  The \code{bbox(object)<-value} method can be used to set
  the bounding-box of a rectangular object. In this case
  the \code{value} argument is expected to be a named list
  (\code{list(x=,y=,width=,heigh=)})

  The \code{x(object)<-value} method can be used to set the
  \emph{X-axis} location of a rectangular object.

  The \code{y(object)<-value} method can be used to set the
  \emph{Y-axis} location of a rectangular object.

  The \code{width(object)<-value} method can be used to set
  the width of an rectangular object.

  The \code{height(object)<-value} method can be used to
  set the height of a rectangular object.
}

