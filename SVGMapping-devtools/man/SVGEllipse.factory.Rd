\name{SVGEllipse.factory}
\alias{SVGEllipse.factory}
\title{SVGEllipse Factory}
\usage{
  SVGEllipse.factory(cx, cy, rx, ry, coords, class, style,
    transform)
}
\arguments{
  \item{coords}{a list of coordinates \code{(cx,cy,rx,ry)}}

  \item{cx}{the X-axis coordinate of the center}

  \item{cy}{the Y-axis coordinate of the center}

  \item{rx}{the X-axis radius length of the ellipse}

  \item{ry}{the Y-axis radius length of the ellipse}

  \item{class}{the CSS class name}

  \item{style}{the CSS style description}

  \item{transform}{the geometric transformation to apply to
  the coordinates}
}
\value{
  an \code{\link{SVGEllipse}} instance
}
\description{
  This function returns an SVG Ellipse instance given its
  coordinates and optionally its class (CSS), style (CSS)
  and a geometric transformation.
}
\details{
  The usage of \code{coords} and \code{(cx,cy,rx,ry)} are
  mutually exclusive.

  If calls with no arguments, this function returns an
  \emph{empty} line (\emph{i.e.} all coordinates are set to
  zero).
}
\examples{
## Simple ellipse
ellipse <- SVGEllipse.factory(cx="3cm",cy="2cm",rx="15cm",ry="10cm")
## Create an ellipse using coordinates like list
el.coords <- list(cx="30px",cy="25px",rx="250px",ry="135px")
ellipse <- SVGEllipse.factory(coords=el.coords)
## An ellipse with some inline CSS decorations
ellipse <- SVGEllipse.factory(coords=el.coords,class="mylines",style="stroke-width:10cm")
## An ellipse (0,0) translated of 20 to the right and 10 below,
## new coordinates are (20,10)
ellipse <- SVGEllipse.factory(cx=0,cy=0,rx=60,ry=30,transform="translate(20,10)")
}

