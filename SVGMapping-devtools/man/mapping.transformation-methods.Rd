\docType{methods}
\name{fnRandom}
\alias{fnIdentity}
\alias{fnIdentity,Mapping-method}
\alias{fnLinear}
\alias{fnLinear,Mapping-method}
\alias{fnLog2FC}
\alias{fnLog2FC,Mapping-method}
\alias{fnLogistic}
\alias{fnLogistic,Mapping-method}
\alias{fnNone}
\alias{fnNone,Mapping-method}
\alias{fnRandom}
\alias{fnRandom,Mapping-method}
\alias{fnRangeLinear}
\alias{fnRangeLinear,Mapping-method}
\alias{fnSigmoid}
\alias{fnSigmoid,Mapping-method}
\alias{fnUser}
\alias{fnUser,Mapping-method}
\title{Mapping Transformation Function}
\arguments{
  \item{.Object}{the mapping instance}

  \item{min}{the minimum parameter of a transformation
  function}

  \item{max}{the maximum parameter of a transformation
  function}

  \item{a}{a parameter of the transformation function}

  \item{b}{a parameter of the transformation functions}

  \item{K}{a parameter of the transformation function}

  \item{r}{a parameter of the transformation function}
}
\value{
  the mapping instance as an invisible object
}
\description{
  These methods allow to set the transformation function,
  either using one of the built-in function or using a
  user-define function.
}
\details{
  The \code{fnRandom(.Object,min,max)} method assigns the
  \code{\link{MAPPING.Random}} function to the current
  mapping instance.

  The \code{fnIdentity(.Object)} is a trivial
  transformation function that returns the input value as
  without any modifications.

  The \code{fnLinear(.Object,a,b)} method assigns the
  \code{\link{MAPPING.Linear}} transformation function to
  the current mapping instance.

  The \code{fnRangeLinear(.Object,a,b,min,max)} method
  assigns the \code{\link{MAPPING.RangeLinear}}
  transformation function to the current mapping instance.

  The \code{fnLogistic(.Object,K,a,r)} method assigns the
  \code{\link{MAPPING.Logistic}(.Object,K,a,r)}
  transformation function to the current mapping instance.

  The \code{fnSigmoid(.Object,r)} method assigns the
  \code{\link{MAPPING.Sigmoid}} transformation function to
  the mapping instance.

  The \code{fnLog2FC(.Object)} method assigns the
  \code{\link{MAPPING.Log2FC}} transformation function to
  the mapping instance.

  The \code{fnUser(.Object,fn,fn.params)} method assigns a
  user-defined transformation function to the mapping
  instance. The \code{fn} parameter is a transformation
  function with the prototype \code{function(x,p)}. The
  \code{fn.params} is the function parameters named list.

  The \code{fnNone(.Object)} remove any previous
  transformation function assignment. Thus, the input data
  are not processed by a function prior to the mapping
  operation. Compare to \code{fnIdentity}, which at the end
  gives a similar output, the \code{fnNone} is much more
  efficient because there is no function called.
}

