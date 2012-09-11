\docType{methods}
\name{targetUnit}
\alias{targetUnit}
\alias{targetUnit,MappingValues-method}
\alias{targetUnit<-}
\alias{targetUnit<-,MappingValues-method}
\title{Target Unit Accessor}
\arguments{
  \item{object}{the mapping instance object}
}
\value{
  the unit value as a character string
}
\description{
  Target units are used to postfix outputs of a mapping
  values operation. This is useful to specify units
  (\emph{ie} after a conversion transformation function).
}
\details{
  There is no need to add an extra space in the unit
  specification, it will be automatically added during the
  mapping process.

  The \code{targetUnit(object)} method retrieves the
  current target unit value.

  The \code{targetUnit(object) <- value} method sets the
  target unit value of the \code{object} mapping instance
}

