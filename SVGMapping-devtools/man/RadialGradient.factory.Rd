\name{RadialGradient.factory}
\alias{RadialGradient.factory}
\title{Radial Gradient Factory}
\usage{
  RadialGradient.factory(..., stops, bbox, fx, fy,
    spread.method)
}
\arguments{
  \item{...}{all unnamed arguments are considered as
  gradient stops}

  \item{stops}{a vector of color stops as
  \code{\link{GradientStop}} instances}

  \item{bbox}{a list of \code{\link{Circle}} coordinates
  \code{(cx,cy,r)}}

  \item{fx}{the \emph{X-axis} focal point definition}

  \item{fy}{the \emph{Y-axis} focal point definition}

  \item{spread.method}{indicates what happens if the
  gradient starts or ends inside the bounds of the target
  \emph{shape}.}
}
\value{
  an \code{\link{RadialGradient}} instance
}
\description{
  This function returns a RadialGradient instance given its
  coordinates and core attributes.
}
\details{
  The gradient can be previewed with the system browser
  using the \code{\link{show}} method.

  Once defined a gradient can be inserted into the document
  definition using the \code{definition} method of the
  \code{SVG} class. After insertion an identifier is
  assigned to gradient and can be retrieve using the
  \code{\link{id}} or \code{\link{URL}} to get an already
  forge document URL.
}
\examples{
## create and empy SVG document
## svg <- SVG.factory(dim="a4")
## First, we define two color stops: blue and red (opacity 50\%)
blue <- GradientStop.factory(color="blue")
red.50 <- GradientStop.factory(offset="1",color="red",opacity=0.5)
## Radial blue-red Linear Gradient with a radius of 40\%
gradient <- RadialGradient.factory(stops=c(blue,red.50), bbox=list(cx="0",cy="0",r="40\%"))
## add the gradient to the document definition
## definition(svg) <- gradient
## retrieve the gradient url
gradient.url <- URL(gradient)
}
\seealso{
  The \code{\link{RadialGradient}} class definition and the
  \code{\link{GradientStop}} class used to defined each
  color stops.
}

