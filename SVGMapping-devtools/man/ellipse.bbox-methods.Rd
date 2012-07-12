\docType{methods}
\name{bbox}
\alias{bbox}
\alias{bbox,Ellipse-method}
\alias{bbox<-}
\alias{bbox<-,Ellipse-method}
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
\arguments{
  \item{object}{an ellipse object}
}
\value{
  \code{bbox()}: a named list (\code{list(cx,cy,rx,ry)}) of
  values for the complete bounding-box given as strings.

  coordinates in units compliant with the SVG 1.1
  specifications
}
\description{
  These methods are accessors to the center and radius of
  an Ellipse object.
}
\details{
  The method \code{bbox(object)} return the bounding box
  (cx,cy,rx,ry) of the ellipse object.

  Coordinates or radius length are given as values
  compliant with the SVG 1.1 specifications

  The \code{bbox{object} <- value} method sets the center
  and radius length of an ellipse object. It is expected
  that \code{value} is a list containing the four named
  values \code{(cx,cy,rx,ry)}.

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

