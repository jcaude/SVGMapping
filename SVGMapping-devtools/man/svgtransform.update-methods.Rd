\docType{methods}
\name{update<-}
\alias{update<-}
\alias{update<-,SVGTransform-method}
\title{Update CTM}
\arguments{
  \item{object}{is an SVGTransformation object}
}
\description{
  This method update the current transform matrix given a
  parent ctm. CTM are used to calculate the viewport
  coordinates after applying a hierarchical set of
  geometric transformations.
}
\details{
  The \code{update(object)<-} method returns nothing. If
  you want to retrieve the updated CTM matrix, use the
  \code{\link{ctm}} method.
}

