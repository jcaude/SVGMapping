\docType{methods}
\name{offset}
\alias{color}
\alias{color,GradientStop-method}
\alias{color<-}
\alias{color<-,GradientStop-method}
\alias{offset}
\alias{offset,GradientStop-method}
\alias{offset<-}
\alias{offset<-,GradientStop-method}
\title{Gradient Stop Accessors}
\arguments{
  \item{object}{is a Gradient stop instance}
}
\value{
  a string (\code{offset} and \code{color}) or a numeric
  value (\code{opacity})
}
\description{
  These methods allow to get/set attributes of a Gradient
  Stop.
}
\details{
  The \code{offset(object)} method returns the offset value
  of a Gradient stop.

  The \code{offset(object) <- value} method set the offset
  value of a Gradient stop. This value must be a compatible
  offset as defined in the SVG 1.1 specifications.

  The \code{color(object)} method returns the color value
  of a Gradient stop.

  The \code{color(object) <- value} method set the color
  value of a Gradient stop. This value must be a compatible
  offset as defined in the SVG 1.1 specifications.
}

