\name{Mask.factory}
\alias{Mask.factory}
\title{Mask Factory}
\usage{
  Mask.factory(x, y, width, height, mask.units,
    mask.content.units, mask.content)
}
\arguments{
  \item{x}{the x-axis location as an \code{\link{SVGCoord}}
  value}

  \item{y}{the y-axis location as an \code{\link{SVGCoord}}
  value}

  \item{width}{the width length as an
  \code{\link{SVGLength}} value}

  \item{height}{the height length as an
  \code{\link{SVGLength}} value}

  \item{mask.units}{the mask units system. See
  \code{\link{maskUnits}} for details.}

  \item{mask.content.units}{the mask content units system.
  See \code{\link{maskContentUnits}} for details.}

  \item{mask.content}{the content, shapes, of the mask. See
  \code{\link{maskContent}} for details.}
}
\value{
  a \code{\link{Mask}} object
}
\description{
  This function returns a new \code{\link{Mask}} object.
}
\details{
  \code{Mask} objects are used to specify Masks that are
  used to alter a shape or a group of shapes.
}
\examples{
## default mask
Mask.factory()
}

