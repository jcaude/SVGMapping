\docType{methods}
\name{maskContentUnits}
\alias{maskContentUnits}
\alias{maskContentUnits,Mask-method}
\alias{maskContentUnits<-}
\alias{maskContentUnits<-,Mask-method}
\title{Mask Content Units Accessors}
\arguments{
  \item{object}{is the Mask object}
}
\value{
  the mask content units value
}
\description{
  These methods define the coordinate system for the
  contents of the mask.

  The \code{maskContentUnits(object) <- value} sets the
  mask content units of the \emph{object} Mask instance.
}
\details{
  The \code{maskContentUnits(object)} method retrieves the
  current mask content untis value of \emph{object}.
}
\note{
  If it is equal to \emph{userSpaceOnUse} (by default),
  then the user coordinate system for the contents of the
  ‘mask’ element is the current user coordinate system in
  place at the time when the ‘mask’ element is referenced
  (\emph{ie}, the user coordinate system for the element
  referencing the ‘mask’ element via the ‘mask’ property).

  If it is equal to \emph{objectBoundingBox}, then the user
  coordinate system for the contents of the ‘mask’ is
  established using the bounding box of the element to
  which the mask is applied.
}

