\docType{methods}
\name{roundx}
\alias{roundx}
\alias{roundx,SVGRect-method}
\alias{roundx<-}
\alias{roundx<-,SVGRect-method}
\alias{roundy}
\alias{roundy,SVGRect-method}
\alias{roundy<-}
\alias{roundy<-,SVGRect-method}
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
  The \code{roundx(object)} method returns the
  \emph{X-axis} radius of the ellipse used to round off the
  corners of the shape.

  The \code{roundx(object) <- value} method can be used to
  set the \emph{X-axis} radius of the ellipse used to round
  off the corners of the shape.

  The \code{roundy(object)} method returns the
  \emph{Y-axis} radius of the ellipse used to round off the
  corners of the shape.

  The \code{roundy(object) <- value} method can be used to
  set the \emph{Y-axis} radius of the ellipse used to round
  off the corners of the shape.
}

