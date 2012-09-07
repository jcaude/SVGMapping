\docType{methods}
\name{gradientType}
\alias{fillAngle}
\alias{fillAngle,MappingColors-method}
\alias{fillAngle<-}
\alias{fillAngle<-,MappingColors-method}
\alias{gradientType}
\alias{gradientType,MappingColors-method}
\alias{gradientType<-}
\alias{gradientType<-,MappingColors-method}
\title{Gradient Specification Accessors}
\description{
  When using multivariate input values (see the
  \code{\link{MappingColors}} class definition) the mapping
  operation results in the assignment of a gradient to the
  target style attribute. The methods described here allow
  to specify the type of gradient (either linear or radial)
  and the filling angle.
}
\details{
  The \code{gradientType(object)} method returns the type
  of gradient, either \emph{linear} or \emph{radial},
  generated.

  The \code{gradientType(object) <- value} method sets the
  type of gradient, here \emph{value} must be either
  \code{'linear'} or \code{'radial'}, to generate.

  The \code{fillAngle(object)} method returns the filling
  angle of the gradient.

  The \code{fillAngle(object) <- value} method sets the
  filling angle value of the gradient to generate.
}

