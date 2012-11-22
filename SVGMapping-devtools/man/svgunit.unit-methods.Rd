\docType{methods}
\name{uValue}
\alias{uUnits}
\alias{uUnits,SVGUnit-method}
\alias{uUnits<-}
\alias{uUnits<-,SVGUnit-method}
\alias{uValue}
\alias{uValue,SVGUnit-method}
\alias{uValue<-}
\alias{uValue<-,SVGUnit-method}
\title{Unit Value Accessors}
\arguments{
  \item{object}{is an SVGUnit object}
}
\value{
  \code{uValue} returns a numeric unit value

  \code{uUnits} returns a character unit system abreviation
}
\description{
  These methods are accessors to the unit value and units
  sytem of a \emph{SVGUnit} object. Valid units system are
  defined in the SVG 1.1 specification.
}
\details{
  The \code{uValue(object)} method returns the unit value
  in the current unit system.

  The \code{uValue(object) <- value} method can be used to
  set the unit value of a \emph{SVGUnit} object

  The \code{uUnits(object)} method returns the unit system
  of the current unit object.

  The \code{uUnits(object) <- value} method can be used to
  set the unit system of a \emph{SVGUnit} object
}

