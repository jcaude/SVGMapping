\docType{methods}
\name{rx}
\alias{rx}
\alias{rx,SVGRect-method}
\alias{rx<-}
\alias{rx<-,SVGRect-method}
\alias{ry}
\alias{ry,SVGRect-method}
\alias{ry<-}
\alias{ry<-,SVGRect-method}
\title{Rectangle round off Corners accessors}
\arguments{
  \item{object}{the SVG shape instance}
}
\value{
  the length in SVG units
}
\description{
  The methods get or set the round off corners of an SVG
  Shape.
}
\details{
  The \code{rx(object)} method returns the \emph{X-axis}
  radius of the ellipse used to round off the corners of
  the shape.

  The \code{rx(object) <- value} method can be used to set
  the \emph{X-axis} radius of the ellipse used to round off
  the corners of the shape.

  The \code{ry(object)} method returns the \emph{Y-axis}
  radius of the ellipse used to round off the corners of
  the shape.

  The \code{ry(object) <- value} method can be used to set
  the \emph{Y-axis} radius of the ellipse used to round off
  the corners of the shape.
}

