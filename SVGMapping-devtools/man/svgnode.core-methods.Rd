\docType{methods}
\name{svgTransform}
\alias{svgTransform}
\alias{svgTransform,SVGNode-method}
\alias{svgTransform<-}
\alias{svgTransform<-,SVGNode-method}
\title{Node Core Attributes Accessors}
\arguments{
  \item{object}{an SVG Node}
}
\value{
  a character string containing the attribute value
}
\description{
  These methods are accessors to the \emph{core} attributes
  of an SVG node. These attributes are the one defined in
  the SVG 1.1 specifications.
}
\details{
  The \code{svgTransform(object)} method returns the SVG
  \emph{transform} attributes of an SVG node

  The \code{svgTransform(object) <- value} method sets the
  SVG \emph{transform} attributes of an SVG node
}

