\docType{methods}
\name{uDpi}
\alias{uDpi}
\alias{uDpi,SVGUnit-method}
\alias{uDpi<-}
\alias{uDpi<-,SVGUnit-method}
\title{Device Resolution Accessors}
\arguments{
  \item{object}{is an SVGUnit object}
}
\value{
  a numeric dpi value
}
\description{
  These methods are accessors to the device resolution of a
  \emph{SVGUnit} object. This resolution is given in the
  standard pixels by inches unit.
}
\details{
  The default DPI value is 90.

  The \code{uDpi(object)} method returns the device
  resolution of the current unit object.

  The \code{uDpi(object) <- value} method can be used to
  set the device resolution of a \emph{SVGUnit} object.
}

