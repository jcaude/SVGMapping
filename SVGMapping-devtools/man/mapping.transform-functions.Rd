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

  \code{MAPPING.Linear} is used to apply a linear
  transformation, \deqn{y=p_a \time x+p_b}{y=p$a*x+p$b}.

  \code{MAPPING.RangeLinear} is used to apply a bound
  linear transformation. \eqn{y = \begin{cases} p_{min} &
  \text{if $(p_a x+b) \leq p_{min}$} \\ p_{max} & \text{if
  $(p_a x+b) \geq p_{max}$} \\ p_a x+b & \text{otherwise}
  \end{cases} }{y = p$min (if ax+b<p$min) ; p$max (if
  ax+b>p$max); ax+b (otherwise)}

  \code{MAPPING.Logistic}

  \code{MAPPING.Sigmoid}

  \code{MAPPING.Log2FC}
}

