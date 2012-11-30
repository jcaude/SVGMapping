\name{SVGCoord.factory}
\alias{SVGCoord.factory}
\title{SVGCoord Factory}
\usage{
  SVGCoord.factory(x, unit, dpi, target.unit)
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
  an \code{\link{SVGCoord}} object
}
\description{
  This function returns a new \code{\link{SVGCoord}}
  object. \code{SVGCoord} is a subclass of
  \code{\link{SVGUnit}} that allows relative units such as
  \emph{'em'}, \emph{'ex'} or \emph{'\%'}.
}
\details{
  \code{SVGCoord} objects are used to specify coordinates
  of some shape attributes.
}
\examples{
SVGCoord.factory(1.5)
SVGCoord.factory(7,"\%")
SVGCoord.factory(10,"px",dpi=100)
SVGCoord.factory("10.43em")
SVGCoord.factory(0.9,"in",target.unit="px")
SVGCoord.factory(1.5,"cm") - SVGCoord.factory(70,"mm")
}
\seealso{
  The \code{\link{SVGLengthfactory}} function.
}

