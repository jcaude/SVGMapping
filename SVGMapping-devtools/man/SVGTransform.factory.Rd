\name{SVGTransform.factory}
\alias{SVGTransform.factory}
\title{SVGTransform Factory}
\usage{
  SVGTransform.factory(transform = "")
}
\arguments{
  \item{transform}{the SVG transform list as a string}
}
\value{
  a \code{\link{SVGTransform}} object
}
\description{
  This function returns a new \code{\link{SVGTransform}}
  object. \code{SVGTransform} allows to describe
  geometrical transformation that can applied to a (group)
  SVG graphical object(s).
}
\examples{
# a skew x=4.2,y=3.1 transformation given an as css instruction
SVGTransform.factory("skewY(3.1) skewX(4.2)")
}

