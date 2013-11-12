\docType{methods}
\name{coords<-}
\alias{coords<-}
\alias{coords<-,Vector-method}
\alias{x1}
\alias{x1,Vector-method}
\alias{x1<-}
\alias{x1<-,Vector-method}
\alias{x2}
\alias{x2,Vector-method}
\alias{x2<-}
\alias{x2<-,Vector-method}
\alias{y1}
\alias{y1,Vector-method}
\alias{y1<-}
\alias{y1<-,Vector-method}
\alias{y2}
\alias{y2,Vector-method}
\alias{y2<-}
\alias{y2<-,Vector-method}
\title{Coordinates of the Vector object}
\description{
  These methods are accessors to the location and direction
  of a \code{Vector} object
}
\details{
  The \code{coords{object} <- value} method sets the
  coordinates and direction of a vector object. It is
  expected that \code{value} is a list containing four
  \link{SVGCoord} values named \code{list(x1,y1,x2,y2)}.

  The \code{x1(object)} method returns the X-axis
  coordinate of the vector.

  The \code{x1(object) <- value} method sets the X-axis
  coordinate of the vector.

  The \code{y1(object)} method returns the Y-axis
  coordinate of the vector.

  The \code{y1(object) <- value} method sets the Y-axis
  coordinate of the vector.

  The \code{x2(object)} method returns the X-axis direction
  of the vector.

  The \code{x2(object) <- value} method sets the X-axis
  direction of the vector.

  The \code{y2(object)} method returns the Y-axis direction
  of the vector.

  The \code{y2(object) <- value} method sets the Y-axis
  direction of the vector.
}

