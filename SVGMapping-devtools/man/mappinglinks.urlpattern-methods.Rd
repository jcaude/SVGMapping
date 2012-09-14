\docType{methods}
\name{urlPattern}
\alias{urlPattern}
\alias{urlPattern,MappingLinks-method}
\alias{urlPattern<-}
\alias{urlPattern<-,MappingLinks-method}
\title{URL Pattern Accessor}
\arguments{
  \item{object}{the mapping instance object}
}
\value{
  the url pattern value as a character string
}
\description{
  URL patterns are used to process input values, using the
  \code{\link{sprintf}} function, to forge the final URLs.
  This is very handy (it reduce data duplcation) and less
  error prone (url pattern is unique for all input values).
}
\details{
  The \code{urlPattern(object)} method retrieves the
  current URL pattern value.

  The \code{urlPattern(object) <- value} method sets the
  url pattern value of the \code{object} mapping instance
}

