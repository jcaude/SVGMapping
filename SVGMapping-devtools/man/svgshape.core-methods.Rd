\docType{methods}
\name{svgTransform}
\alias{svgTransform}
\alias{svgTransform,SVGShape-method}
\alias{svgTransform<-}
\alias{svgTransform<-,SVGShape-method}
\title{Shape Core Attributes Accessors}
\arguments{
  \item{object}{an SVG shape}
}
\value{
  an \link{SVGTransform} object
}
\description{
  These methods are accessors to the \emph{core} attributes
  of an SVG shape. These attributes are the one defined in
  the SVG 1.1 specifications.
}
\details{
  The \code{svgTransform(object)} method returns the SVG
  \emph{transform} attributes of an SVG shape

  The \code{svgTransform(object) <- value} method sets the
  SVG \emph{transform} attributes of an SVG shape.
  \emph{value} must be a valid \link{SVGTransform} object
  or a compliant definition as a character string. In the
  latter, it will be automatically converted into an
  \link{SVGTransform} object.
}

