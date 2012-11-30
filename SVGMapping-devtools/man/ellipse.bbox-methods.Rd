\docType{methods}
\name{cx}
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
\title{Coordinates of the Ellipse object}
\value{
  coordinates as SVGCoords (\code{cx,cy}) or SVGLength
  (\code{rx,ry}) objects
}
\description{
  These methods are accessors to the center and radius of
  an Ellipse object.
}
\details{
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

