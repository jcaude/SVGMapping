\docType{methods}
\name{transforms}
\alias{transforms}
\alias{transforms,SVGTransform-method}
\alias{transforms<-}
\alias{transforms<-,SVGTransform-method}
\title{Transforms Accessors}
\arguments{
  \item{object}{is an SVGTransformation object}
}
\value{
  \code{transforms} returns a vector of characters
}
\description{
  These methods are accessors to the list of transform
  operations stored as a vector of character strings.
}
\details{
  The \code{transform(object)} method returns the current
  vector of transformations

  The \code{transforms()<-} method sets the vector of
  transformations of a SVGTransform object.
}

