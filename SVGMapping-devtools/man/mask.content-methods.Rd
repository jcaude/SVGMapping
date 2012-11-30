\docType{methods}
\name{maskContent}
\alias{maskContent}
\alias{maskContent<-}
\title{Mask Content Accessors}
\arguments{
  \item{object}{is the Mask instance}
}
\value{
  an XMLInternalNode
}
\description{
  These methods give access of the content (graphical
  elements) of the mask
}
\details{
  The \code{maskContent(object)} method returns the content
  of the mask as an XMLInternalNode

  The \code{maskContent(object) <- value} method set the
  content of the mask. Here \emph{value} is an
  XMLInternalNode.
}
\note{
  The child elements of the \emph{mask} are rendered into
  an offscreen image which has been initialized to
  transparent black. Any graphical object which
  uses/references the given \emph{mask} element will be
  painted onto the background through the mask, thus
  completely or partially masking out parts of the
  graphical object.
}

