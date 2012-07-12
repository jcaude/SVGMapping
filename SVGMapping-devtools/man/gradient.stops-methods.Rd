\docType{methods}
\name{stops}
\alias{stops}
\alias{stops,Gradient-method}
\alias{stops<-}
\alias{stops<-,Gradient-method}
\title{Gradient Stops}
\arguments{
  \item{object}{is an SVG Gradient object}
}
\value{
  the list of gradient stops
}
\description{
  A Gradient is made of several stops. Each stop is
  associated with a relative position, a color and an
  opacity level. This list is used to render a shape.
}
\details{
  The \code{stops(object)} method returns the list of
  gradient stops.

  The \code{stops(object) <- value} method set the list of
  gradient stops of the gradient.
}

