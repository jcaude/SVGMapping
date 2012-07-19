\docType{methods}
\name{transFunction}
\alias{transFunction}
\alias{transFunction,Mapping-method}
\alias{transFunction<-}
\alias{transFunction<-,Mapping-method}
\alias{transParameters}
\alias{transParameters,Mapping-method}
\alias{transParameters<-}
\alias{transParameters<-,Mapping-method}
\title{Mapping Transformation function accessors}
\arguments{
  \item{object}{the mapping instance}
}
\value{
  a \code{function(x,p)} or the transformation function
  parameters \code{p}.
}
\description{
  Mapping is a two steps procedure. First, data are
  processed by a \emph{transformation} function. Second,
  transformed data are used by the mapping \emph{operation}
  to alter the template content. The \emph{transformation}
  function is applied to the whole input dataset. Input
  datasets can be a data-frame or a vector. In the first
  case data will be applied on each "array" values by
  iteratively process rows and columns. In the second case,
  the transformation function is applied iteratively on
  each vector values. The SVGMapping package provides a set
  of built-in transformation function. But you can easily
  design your own transformation as long as it is compliant
  with the \code{function(x,p)} prototype. Where \code{x}
  is an atomic input value and \code{p} a set of parameters
  given as a list. A complete example of a user
  transformation function is given in the example section
}
\details{
  The \code{transFunction(object)} retrieve the current
  transformation function

  The \code{transFunction(object) <- value} set the
  transformation function for the \code{object} mapping
  instance

  The \code{transParameters(object)} returns the parameters
  list used by the transformation function

  The \code{transParameters(object)<-value} sets the
  parameters list used by the transformation function
}

