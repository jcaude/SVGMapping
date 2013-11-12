\docType{methods}
\name{coords<-}
\alias{coords<-}
\alias{coords<-,Circle-method}
\alias{r}
\alias{r,Circle-method}
\alias{r<-}
\alias{r<-,Circle-method}
\title{Location and dimension of the Circle object}
\description{
  These methods are accessors to the location and dimension
  of a \code{Circle} object
}
\details{
  The \code{coords{object} <- value} method sets the center
  and radius length of a circle object. It is expected that
  \code{value} is a list containing the four named values
  \code{list(cx,cy,r)}.

  The \code{r(object)} method returns the radius length of
  the circle as an \code{\link{SVGLength}} object.

  The \code{r(object) <- value} method sets the radius
  length of the circle.
}

