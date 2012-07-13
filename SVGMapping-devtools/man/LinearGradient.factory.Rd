\name{LinearGradient.factory}
\alias{LinearGradient.factory}
\title{Linear Gradient Factory}
\usage{
  LinearGradient.factory(..., stops, bbox, spread.method)
}
\arguments{
  \item{...}{all unnamed arguments are considered as
  gradient stops}

  \item{stops}{a vector of color stops as
  \code{\link{GradientStop}} instances}

  \item{bbox}{a list of coordinates \code{(x1,y1,x2,y2)}}

  \item{spread.method}{indicates what happens if the
  gradient starts or ends inside the bounds of the target
  \emph{shape}.}
}
\value{
  an \code{\link{LinearGradient}} instance
}
\description{
  This function returns a LinearGradient instance given its
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
## Horizontal blue-red Linear Gradient
gradient <- LinearGradient.factory(stops=c(blue,red.50), bbox=list(x1="0",y1="0",x2="1",y2="0"))
## add the gradient to the document definition
## definition(svg) <- gradient
## retrieve the gradient url
gradient.url <- URL(gradient)
}
\seealso{
  The \code{\link{LinearGradient}} class definition and the
  \code{\link{GradientStop}} class used to defined each
  color stops.
}

