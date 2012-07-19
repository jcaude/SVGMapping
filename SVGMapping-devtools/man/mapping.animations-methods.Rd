\docType{methods}
\name{animations}
\alias{animations}
\alias{animations,Mapping-method}
\alias{animations<-}
\alias{animations<-,Mapping-method}
\alias{animatiosn<-}
\title{Animation Flag}
\arguments{
  \item{object}{the mapping instance}
}
\value{
  a boolean, \code{TRUE} if animation is enable and
  \code{FALSE} otherwise
}
\description{
  In some cases, mapping operations can be animated
  (\emph{eg} opacity fading). At the super class level this
  functionality is enable (or disable) using the animation
  flag.
}
\details{
  The \code{animations(object)} method retrieves the
  animation flag

  The \code{animations(object) <- value} method sets the
  animation flag
}

