\docType{methods}
\name{exec}
\alias{exec}
\alias{exec,Mapping-method}
\alias{exec,MappingColors-method}
\title{Mapping Process Execution}
\arguments{
  \item{.Object}{the mapping instance}

  \item{svg}{the template object reference}
}
\value{
  the mapping object as invisible
}
\description{
  Each mapping class must have an \code{exec(.Object,svg)}
  method, and called the parent class method. The super
  class \code{exec} method is responsible for the data
  values transformation. All class that derived from the
  \code{Mapping} super class must implement an \code{exec}
  method that will perform the mapping operation on the
  template (\code{svg} parameter).
}
\details{
  Transformed values are stored in the \code{.values}
  fields. Thus, one can still access the original values
  using the \code{values(object)} method.

  The \code{exec(object,svg)} method is called to process
  the input \emph{svg} document with the current
  \emph{object} mapping colors instance.
}

