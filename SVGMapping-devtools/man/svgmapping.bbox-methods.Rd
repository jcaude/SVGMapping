\docType{methods}
\name{bbox}
\alias{bbox}
\alias{bbox,Circle-method}
\alias{bbox,Ellipse-method}
\alias{bbox,Rectangle-method}
\alias{bbox,Vector-method}
\alias{bbox<-}
\alias{bbox<-,Circle-method}
\alias{bbox<-,Ellipse-method}
\alias{bbox<-,Rectangle-method}
\alias{bbox<-,Vector-method}
\title{Named list of coordinates and dimensions}
\arguments{
  \item{object}{a geometrical object}
}
\value{
  \code{bbox()}: a named list of coordinates and dimensions
}
\description{
  \code{bbox} methods are accessors to the coordinates and
  dimensions of a geometrical shape.
}
\details{
  \bold{Vector:} the method \code{bbox(object)} returns the
  bounding box \code{list(x1,y1,x2,y2)} of the vector
  object.

  \bold{Vector:} The \code{bbox{object} <- value} method
  sets the coordinates and direction of a vector object. It
  is expected that \code{value} is a list containing the
  four named values \code{list(x1,y1,x2,y2)}.

  \bold{Ellipse:} the method \code{bbox(object)} return the
  bounding box \code{list(cx,cy,rx,ry)} of the ellipse
  object.

  \bold{Ellipse:} the \code{bbox{object} <- value} method
  sets the center and radius length of an ellipse object.
  It is expected that \code{value} is a list containing the
  four named values \code{list(cx,cy,rx,ry)}.

  \bold{Rectangle:} the method \code{bbox(object)} return
  the bounding box \code{list(x,y,width,height)} of the
  rectangular object.

  \bold{Rectangle:} the \code{bbox(object)<-value} method
  can be used to set the bounding-box of a rectangular
  object. In this case the \code{value} argument is
  expected to be a named list
  (\code{list(x=,y=,width=,heigh=)})

  \bold{Circle:} the method \code{bbox(object)} return the
  bounding box \code{list(cx,cy,r)} of the circle object.

  \bold{Circle:} the \code{bbox{object} <- value} method
  sets the center and radius length of a circle object. It
  is expected that \code{value} is a list containing the
  four named values \code{list(cx,cy,r)}.
}
\note{
  coordinates or length are given as values compliant with
  the SVG 1.1 specifications
}

