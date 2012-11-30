\docType{methods}
\name{fx}
\alias{fx}
\alias{fx,RadialGradient-method}
\alias{fx<-}
\alias{fx<-,RadialGradient-method}
\alias{fy}
\alias{fy,RadialGradient-method}
\alias{fy<-}
\alias{fy<-,RadialGradient-method}
\title{Focal point definition of the Radial Gradient}
\arguments{
  \item{object}{a RadialGradient object}
}
\value{
  \code{fx(object)} return the coordinates as an
  \code{\link{SVGCoord}} object. \code{fx(object) <- value}
  return the invisible object.
}
\description{
  These methods are accessors to the focal point definition
  of a radial gradient object.
}
\details{
  The method \code{fx(object)} return the X-axis of the
  focal point. The gradient will be drawn such that the 0\%
  gradient stop is mapped to \code{(fx, fy)}.

  The \code{fx{object} <- value} method sets the X-axis
  coordinates of the focal point.

  If attribute \code{fy} is not specified, \code{fy} will
  coincide with the presentational value of
  \code{\link{cy}} for the element whether the value for
  \code{\link{cy}} was inherited or not.

  The \code{fy{object} <- value} method sets the Y-axis
  coordinates of the focal point.
}

