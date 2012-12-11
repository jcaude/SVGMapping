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
  a character string containing the attribute value
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
}

