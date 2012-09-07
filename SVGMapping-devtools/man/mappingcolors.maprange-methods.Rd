\docType{methods}
\name{mapRange.min}
\alias{mapRange}
\alias{mapRange,MappingColors-method}
\alias{mapRange.max}
\alias{mapRange.max,MappingColors-method}
\alias{mapRange.max<-}
\alias{mapRange.max<-,MappingColors-method}
\alias{mapRange.min}
\alias{mapRange.min,MappingColors-method}
\alias{mapRange.min<-}
\alias{mapRange.min<-,MappingColors-method}
\alias{mapRange<-}
\alias{mapRange<-,MappingColors-method}
\title{Mapping Input Range Accessors}
\arguments{
  \item{object}{the mapping colors instance object}
}
\description{
  \code{MappingColors} operation consists of a linear
  mapping between \emph{values}, in a given range, and a
  list of colors. The following methods let get/set the
  input range of the (linear) mapping.
}
\details{
  The mapping range is given as a numeric interval with
  lower and upper bounds. If input \emph{values} are not
  included in the mapping range, then they are
  automatically bounded to this range. Thus, values outside
  of the input range are mapped to the first or last
  mapping colors.

  The \code{mapRange.min(object)} method returns the lower
  bound of the mapping range.

  The \code{mapRange.min(object) <- value} method sets the
  lower bound of a mapping range instance.

  The \code{mapRange.max(object)} method returns the upper
  bound of the mapping range.

  The \code{mapRange.max(object) <- value} method sets the
  upper bound of a mapping range instance.

  The \code{mapRange(object)} methods returns, as a vector,
  the lower and upper bounds of the mapping range.

  The \code{mapRange(object) <- value} methods sets the
  lower and upper bounds, stored in the vector
  \emph{value}, of a mapping colors instance.
}

