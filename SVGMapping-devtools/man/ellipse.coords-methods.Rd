\docType{methods}
\name{coords<-}
\alias{coords<-}
\alias{coords<-,Ellipse-method}
\alias{cx}
\alias{cx,Ellipse-method}
\alias{cx<-}
\alias{cx<-,Ellipse-method}
\alias{cy}
\alias{cy,Ellipse-method}
\alias{cy<-}
\alias{cy<-,Ellipse-method}
\alias{rx}
\alias{rx,Ellipse-method}
\alias{rx<-}
\alias{rx<-,Ellipse-method}
\alias{ry}
\alias{ry,Ellipse-method}
\alias{ry<-}
\alias{ry<-,Ellipse-method}
\title{Location and dimensions of the Ellipse object}
\value{
  coordinates as SVGCoords (\code{cx,cy}) or SVGLength
  (\code{rx,ry}) objects
}
\description{
  These methods are accessors to the location and
  dimensions of an \code{Ellipse} object
}
\details{
  The \code{coords{object} <- value} method sets the
  location of the center and radius length of an ellipse
  object. It is expected that \code{value} is a list
  containing the four named values
  \code{list(cx,cy,rx,ry)}.

  The \code{cx(object)} method returns the X-axis center of
  the ellipse.

  The \code{cx(object) <- value} method sets the X-axis
  center of the ellipse.

  The \code{cy(object)} method returns the Y-axis center of
  the ellipse.

  The \code{cy(object) <- value} method sets the Y-axis
  center of the ellipse.

  The \code{rx(object)} method returns the X-axis radius
  length of the ellipse.

  The \code{rx(object) <- value} method sets the X-axis
  radius length of the ellipse.

  The \code{ry(object)} method returns the Y-axis radius
  length of the ellipse.

  The \code{ry(object) <- value} method sets the Y-axis
  radius length of the ellipse.
}

