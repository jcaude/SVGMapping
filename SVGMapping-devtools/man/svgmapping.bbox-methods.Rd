\docType{methods}
\name{bbox}
\alias{bbox}
\alias{bbox,Circle-method}
\alias{bbox,Ellipse-method}
\alias{bbox,Rectangle-method}
\alias{bbox,Vector-method}
\title{Bounding-Box method}
\arguments{
  \item{object}{a geometrical object}
}
\value{
  \code{bbox()}: a named list of coordinates and dimensions
}
\description{
  \code{bbox} methods returns the bounding-box (i.e
  location of the upper-left and bottom-right corners of
  the minimal rectangle that contains the geometric
  item(s)) of a geometrical shape.

  <title already defined>

  <title already defined>
}
\details{
  \bold{Circle:} the method \code{bbox(object)} return the
  bounding box \code{list(cx,cy,r)} of the circle object.
}
\note{
  coordinates are given as \link{SVGCoord} objects
}

