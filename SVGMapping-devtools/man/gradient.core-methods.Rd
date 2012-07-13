\docType{methods}
\name{svgUnits}
\alias{spreadMethod}
\alias{spreadMethod,Gradient-method}
\alias{spreadMethod<-}
\alias{spreadMethod<-,Gradient-method}
\alias{svgUnits}
\alias{svgUnits,Gradient-method}
\alias{svgUnits<-}
\alias{svgUnits<-,Gradient-method}
\alias{xlinkHref}
\alias{xlinkHref,Gradient-method}
\alias{xlinkHref<-}
\alias{xlinkHref<-,Gradient-method}
\title{Gradient Core Attributes Accessors}
\arguments{
  \item{object}{is an SVG Gradient object}
}
\value{
  a core attribute as a string
}
\description{
  These methods are accessors to the \emph{core} attributes
  of an SVG Gradient object. These attributes are described
  in the SVG 1.1 specifications document.
}
\details{
  The \code{svgUnits(object)} method returns the SVG
  \emph{units} attribute of an SVG Gradient.

  The \code{svgUnits(object) <- value} method sets the SVG
  \emph{units} attribute of an SVG Gradient. The
  \emph{units} value must be etheir \code{userSpaceOnUse}
  or \code{objectBoundingBox} (see SVG specifications for
  details).

  The \code{spreadMethod(object)} method returns the SVG
  \emph{spreadMethod} attribute of an SVG Gradient.

  The \code{spreadMethod(object) <- value} method sets the
  SVG \emph{spreadMethod} attribute of an SVG Gradient. The
  \emph{spreadMethod} value must be etheir \code{pad},
  \code{reflect} or \code{repeat} (see SVG specifications
  for details).

  The \code{xlinkHref(object)} method returns the SVG
  \emph{xlink:href} attribute of an SVG Gradient.

  The \code{xlinkHref(object) <- value} method sets the SVG
  \emph{xlink:href} attribute of an SVG Gradient.
}

