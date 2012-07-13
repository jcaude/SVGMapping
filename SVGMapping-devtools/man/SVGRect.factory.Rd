\name{SVGRect.factory}
\alias{SVGRect.factory}
\title{SVGRect Factory}
\usage{
  SVGRect.factory(x, y, width, height, bbox, roundx,
    roundy, class, style, transform)
}
\arguments{
  \item{bbox}{a list of coordinates
  \code{(x,y,width,height)}}

  \item{x}{the X-axis coordinate of the upper left corner}

  \item{y}{the Y-axis coordinate of the upper left corner}

  \item{width}{the horizontal length of the rectangle}

  \item{height}{the vertical length of the rectangle}

  \item{roundx}{the X-avis corner round-off length}

  \item{roundy}{the Y-axis corner round-off length}

  \item{class}{the CSS class name}

  \item{style}{the CSS style description}

  \item{transform}{the geometric transformation to apply to
  the coordinates}
}
\value{
  an \code{\link{SVGRect}} instance
}
\description{
  This function returns an SVG Rectangle instance given its
  coordinates and optionally its class (CSS), style (CSS)
  and a geometric transformation.
}
\details{
  The usage of \code{bbox} and \code{(x,y,width,height)}
  are mutually exclusive.

  If calls with no arguments, this function returns an
  \emph{empty} line (\emph{i.e.} all coordinates are set to
  zero).
}
\examples{
## Simple rectangle
rect <- SVGRect.factory(x="5cm",y="5cm",width="60cm",height="30cm",roundx="2px", roundy="1px")
## Create a rectangle using bounding-box list
rect.bbox <- list(x="5px",y="5px",width="650px",height="315px")
rect <- SVGRect.factory(bbox=rect.bbox)
## A rectangle with some inline CSS decorations
rect <- SVGRect.factory(bbox=rect.bbox,class="mylines",style="stroke-width:15cm")
## A rectangle (0,0) translated of 10 to the right and 20 below,
## new coordinates are (10,20)
rect <- SVGRect.factory(x=0,y=0,width=60,height=30,transform="translate(10,20)")
}
\seealso{
  The \code{\link{SVGRect}} class definition; the
  \code{\link{roundx}} and \code{\link{roundy}} methods to
  set the round off corners length.
}

