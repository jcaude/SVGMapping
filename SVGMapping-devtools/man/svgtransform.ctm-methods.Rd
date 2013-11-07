\docType{methods}
\name{ctm}
\alias{ctm}
\alias{ctm,SVGTransform-method}
\title{CTM Matrix Accessors}
\arguments{
  \item{object}{is an SVGTransformation object}
}
\value{
  \code{ctm} returns a numeric matrix
}
\description{
  This method is a getter to the Current Transformation
  Matrix (CTM) of a \emph{SVGTransform} object. CTM are 3x3
  numeric matrices as defined in the SVG 1.1 specification.
  Transformation matrices are of the form:
}
\details{
  m = [ [a c e] [b d f] [0 0 1] ]

  where [a b c d e f] are the transformation parameters

  The \code{ctm(object)} method returns the current
  transformation matrix
}

