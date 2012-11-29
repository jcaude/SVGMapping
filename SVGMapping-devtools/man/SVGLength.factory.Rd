\name{SVGLength.factory}
\alias{SVGLength.factory}
\title{SVGLength Factory}
\usage{
  SVGLength.factory(x, unit, dpi, target.unit)
}
\arguments{
  \item{x}{the unit value}

  \item{unit}{the value unit system}

  \item{dpi}{the device resoluation}

  \item{target.unit}{the return unit system after value
  conversion. Note that only non relative source units can
  be converted into another unit system.}
}
\value{
  a \code{\link{SVGLength}} object
}
\description{
  This function returns a new \code{\link{SVGLength}}
  object. \code{SVGLength} is a subclass of
  \code{\link{SVGUnit}} that allows relative units such as
  \emph{'em'}, \emph{'ex'} or \emph{'\%'}.
}
\details{
  \code{SVGLength} objects are used to specify length of
  some shape attributes.
}
\examples{
SVGLength.factory(1.5)
SVGLength.factory(7,"\%")
SVGLength.factory(10,"px",dpi=100)
SVGLength.factory("10.43em")
SVGLength.factory(0.9,"in",target.unit="px")
SVGLength.factory(1.5,"cm") - SVGLength.factory(70,"mm")
}
\seealso{
  The \code{\link{SVGUnit.factory}} function.
}

