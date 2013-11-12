\docType{methods}
\name{coords<-}
\alias{coords<-}
\alias{coords<-,Rectangle-method}
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
\title{Location and dimensions of the Rectangle object}
\value{
  \code{x(),y()}: an SVGCoord object for single coordinates
  and an SVGLength object for \code{width(),height()}
  dimensions methods

  \code{x(),y(),width(),height()<-}: the rectangular object
  as invisible
}
\description{
  These methods are accessors to the location and
  dimensions of a \code{Rectangle} object
}
\details{
  The \code{coords{object} <- value} method can be used to
  set the location and dimensions of a rectangular object.
  In this case the \code{value} argument is expected to be
  a named list (\code{list(x=,y=,width=,height=)})

  The method \code{x(object)} return the location on the
  \emph{X-axis} of the rectangular object.

  The \code{x(object)<-value} method can be used to set the
  \emph{X-axis} location of a rectangular object.

  The method \code{y(object)} return the location on the
  \emph{Y-axis} of the rectangular object.

  The \code{y(object)<-value} method can be used to set the
  \emph{Y-axis} location of a rectangular object.

  The method \code{width(object)} return the width of the
  rectangular object.

  The \code{width(object)<-value} method can be used to set
  the width of an rectangular object.

  The method \code{height(object)} return the width of the
  rectangular object.

  The \code{height(object)<-value} method can be used to
  set the height of a rectangular object.
}

