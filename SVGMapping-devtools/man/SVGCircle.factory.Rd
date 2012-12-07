\name{SVGCircle.factory}
\alias{SVGCircle.factory}
\title{SVGCircle Factory}
\usage{
  SVGCircle.factory(cx, cy, r, bbox, class, style,
    transform)
}
\arguments{
  \item{bbox}{a list of coordinates \code{(cx,cy,r)}}

  \item{cx}{the X-axis coordinate of the center}

  \item{cy}{the Y-axis coordinate of the center}

  \item{r}{the radius length of the circle}

  \item{class}{the CSS class name}

  \item{style}{the CSS style description}

  \item{transform}{the geometric transformation to apply to
  the coordinates}
}
\value{
  an \code{\link{SVGCircle}} instance
}
\description{
  This function returns an SVG Circle instance given its
  coordinates and optionally its class (CSS), style (CSS)
  and a geometric transformation.
}
\details{
  The usage of \code{bbox} and \code{(cx,cy,r)} are
  mutually exclusive.

  If calls with no arguments, this function returns an
  \emph{empty} line (\emph{i.e.} all coordinates are set to
  zero).
}
\examples{
## Simple circle
circle <- SVGCircle.factory(cx="3cm",cy="2cm",r="15cm")
## Create a circle using bounding-box list
el.bbox <- list(cx="30px",cy="25px",r="135px")
circle <- SVGCircle.factory(bbox=el.bbox)
## A circle with some inline CSS decorations
circle <- SVGCircle.factory(bbox=el.bbox,class="mylines",style="stroke-width:10cm")
## A circle (0,0) translated of 20 to the right and 10 below,
## new coordinates are (20,10)
circle <- SVGCircle.factory(cx=0,cy=0,r=30,transform="translate(20,10)")
}

