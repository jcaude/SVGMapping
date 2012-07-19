\name{MAPPING.Random}
\alias{MAPPING.Identity}
\alias{MAPPING.Linear}
\alias{MAPPING.Log2FC}
\alias{MAPPING.Logistic}
\alias{MAPPING.Random}
\alias{MAPPING.RangeLinear}
\alias{MAPPING.Sigmoid}
\title{Mapping built-in transformation functions}
\usage{
  MAPPING.Random(x, p)

  MAPPING.Identity(x, p)

  MAPPING.Linear(x, p)

  MAPPING.RangeLinear(x, p)

  MAPPING.Logistic(x, p)

  MAPPING.Sigmoid(x, p)

  MAPPING.Log2FC(x, p)
}
\arguments{
  \item{x}{the input value}

  \item{p}{the function parameters list}
}
\value{
  the transformed value which can be of any type (\emph{eg}
  character, numeric,...)
}
\description{
  SVGMapping comes with a set of predifined
  \emph{transformation} functions. \emph{transformation}
  function are applied to the input values. Still, one can
  implement it's own function.
}
\details{
  \code{MAPPING.Random} returns random values (whatever the
  input) in the range \code{[p$min,p$max]}.

  \code{MAPPING.Identity} returns the input value without
  doing any modification.

  \code{MAPPING.Linear} is used to apply the linear
  transformation: \deqn{y=p_a x+p_b}{y=p$a*x+p$b}

  \code{MAPPING.RangeLinear} is used to apply a bound
  linear transformation: \deqn{y = p_a x+b }{y = p$a*x+p$b}
  If \eqn{y}{y} is not is the range
  \eqn{[p_{min},p_{max}]}{[p$min,p$max]} then: \deqn{y =
  \max(p_{min},\min(p_{max},y))}{y=max(p$min,min(p$max,y))}

  \code{MAPPING.Logistic} is used to apply the logistic
  transformation: \deqn{ y = \frac{p_k}{1+p_a \exp^{-p_r
  x}}}{y = p$k / (1 + p$a * exp(-p$r * x))}

  \code{MAPPING.Sigmoid} is used to apply the sigmoid
  transformation: \deqn{ y = \frac{1}{1+p_a \exp^{-p_r
  x}}}{y = 1 / (1 + p$a * exp(-p$r * x))}

  \code{MAPPING.Log2FC} is a transformation function
  specifically design for microarrays analysis. It can be
  used to transform log based 2 expression ratios into
  fold-change. The transformation formula depends on the
  sign of the expression ratio. If this ratio is positive,
  the transformation is: \deqn{y = \exp(x \log(2))}{exp(x *
  log(2))} And if the ratio is negative, the transformation
  formula is: \deqn{y= \frac{-1}{\exp(x
  \log(2))}}{-1/(exp(x*log(2)))}
}
\section{Note}{
  One can design its own transformation function as long as
  it uses the standard prototype \code{function(x,p)}.
  Where \code{x} is an atomic value and \code{p} the
  function parameters as a list.
}

