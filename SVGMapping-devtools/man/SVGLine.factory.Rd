\name{SVGLine.factory}
\alias{SVGLine.factory}
\title{SVGLine Factory}
\usage{
  SVGLine.factory(x1, y1, x2, y2, coords, class, style,
    transform)
}
\arguments{
  \item{coords}{a list of coordinates \code{(x1,y1,x2,y2)}}

  \item{x1}{the X-axis coordinate of the first extremum
  point}

  \item{y1}{the Y-axis coordinate of the first extremum
  point}

  \item{x2}{the X-axis coordinate of the second extremum
  point}

  \item{y2}{the Y-axis coordinate of the second extremum
  point}

  \item{class}{the CSS class name}

  \item{style}{the CSS style description}

  \item{transform}{the geometric transformation to apply to
  the coordinates}
}
\value{
  an \code{\link{SVGLine}} instance
}
\description{
  This function returns an SVG Line instance given its
  coordinates and optionally its class (CSS), style (CSS)
  and a geometric transformation.
}
\details{
  The usage of \code{coords} and \code{(x1,y1,x2,y2)} are
  mutually exclusive.

  If calls with no arguments, this function returns an
  \emph{empty} line (\emph{i.e.} all coordinates are set to
  zero).
}
\examples{
## Simple line
line <- SVGLine.factory(x1="10cm",y1="10cm",x2="40cm",y2="15cm")
## Create a line using a list of coordinates
line.coords <- list(x1="5px",y1="10px",x2="650px",y2="315px")
## A line with some CSS
line2 <- SVGLine.factory(coords=line.coords,class="mylines",style="stroke-width:15cm")
## A line (0,0,10,10) translated of 10 to the right and 20 below,
## new coordinates are (10,20,20,30)
line3 <- SVGLine.factory(0,0,10,10,transform="translate(10,20)")
}
\seealso{
  The \code{\link{SVGLine}} class definition
}

