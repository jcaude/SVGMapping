\docType{methods}
\name{maskUnits}
\alias{maskUnits}
\alias{maskUnits,Mask-method}
\alias{maskUnits<-}
\alias{maskUnits<-,Mask-method}
\title{Mask Units Accessors}
\arguments{
  \item{object}{is the Mask object}
}
\value{
  the mask units value as a string
}
\description{
  These methods defines the coordinate system for
  attributes ‘x’, ‘y’, ‘width’ and ‘height’.

  The \code{maskUnits(object) <- value} sets the mask units
  of the \emph{object} Mask instance.
}
\details{
  The \code{maskUnits(object)} method retrieves the current
  mask untis value of \emph{object}.
}
\note{
  If it is equal to \emph{userSpaceOnUse} (by default) then
  ‘x’, ‘y’, ‘width’ and ‘height’ represent values in the
  current user coordinate system in place at the time when
  the ‘mask’ element is referenced (\emph{ie}, the user
  coordinate system for the element referencing the ‘mask’
  element via the ‘mask’ property).

  If it is equal to \emph{objectBoundingBox} then ‘x’, ‘y’,
  ‘width’ and ‘height’ represent fractions or percentages
  of the bounding box of the element to which the mask is
  applied. (See Object bounding box units.)
}

