\docType{methods}
\name{bbox}
\alias{bbox}
\alias{bbox,Circle-method}
\alias{bbox<-}
\alias{bbox<-,Circle-method}
\alias{r}
\alias{r,Circle-method}
\alias{r<-}
\alias{r<-,Circle-method}
\title{Coordinates of the Circle object}
\arguments{
  \item{object}{an circle object}
}
\value{
  \code{bbox()}: a named list (\code{list(cx,cy,r)}) of
  values for the complete bounding-box given as strings.
}
\description{
  These methods are accessors to the center and radius of a
  Circle object.
}
\details{
  The method \code{bbox(object)} return the bounding box
  (cx,cy,r) of the circle object.

  Coordinates or radius length are given as values
  compliant with the SVG 1.1 specifications

  The \code{bbox{object} <- value} method sets the center
  and radius length of a circle object. It is expected that
  \code{value} is a list containing the four named values
  \code{(cx,cy,r)}.

  The \code{r(object)} method returns the radius length of
  the circle.

  The \code{r(object) <- value} method sets the radius
  length of the circle.
}
\seealso{
  \code{\link{cx}} and \code{\link{cy}} method to set the
  center of the circle
}

