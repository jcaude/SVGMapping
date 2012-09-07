\docType{methods}
\name{mapColors}
\alias{mapColors}
\alias{mapColors,MappingColors-method}
\alias{mapColors<-}
\alias{mapColors<-,MappingColors-method}
\title{Mapping Colors vector accessors}
\arguments{
  \item{object}{the mapping colors instance object}
}
\value{
  a vector of color definition (each as a string)
}
\description{
  These methods allow to get/set the list of colors that
  will be used by the \code{MappingColors} operation.
}
\details{
  The list of colors must be a vector of single color
  definition as a string. Each color must be compliant with
  the SVG color definition standard. A good practice is to
  give color as string using the \code{#RRGGBB} format.

  The \code{mapColors(object)} method retrieve the current
  list of mapping colors.

  The \code{mapColors(objec) <- value} method sets the list
  of mapping colors of the \code{object} mapping instance.
}

