\name{SVGUnit.factory}
\alias{SVGUnit.factory}
\title{SVGUnit Factory}
\usage{
  SVGUnit.factory(x, unit, dpi, target.unit)
}
\arguments{
  \item{x}{the unit value}

  \item{unit}{the value unit system}

  \item{dpi}{the device resoluation}

  \item{target.unit}{the return unit system after value
  conversion}
}
\value{
  a \code{\link{SVGUnit}} object
}
\description{
  This function returns a new \code{\link{SVGUnit}} object.
  An SVG unit is simply a single numeric with an associated
  unit system as defined in the SVG Specification.
}
\details{
  \code{SVGUnit} objects are used to specify location or
  shape attributes (\emph{eg} a circle radius). A
  \code{SVGUnit} is specified using a \emph{value}
  associated to a \emph{unit system} and a \emph{device
  resolution} given as a \emph{dpi} value.
}
\examples{
SVGUnit.factory(1.5)
SVGUnit.factory(10,"px")
SVGUnit.factory(10,"px",dpi=100)
SVGUnit.factory("10.43cm")
SVGUnit.factory(0.9,"in",target.unit="px")
SVGUnit.factory(1.5,"cm") - SVGUnit.factory(70,"mm")
}

