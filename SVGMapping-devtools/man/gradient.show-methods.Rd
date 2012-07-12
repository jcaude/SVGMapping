\docType{methods}
\name{show}
\alias{show}
\title{Show method for Gradient}
\arguments{
  \item{object}{is either a \code{\link{LinearGradient}} or
  a \code{\link{RadialGradient}} instance.}
}
\description{
  The \code{show()} method called with a gradient object as
  an argument allows to preview the gradient rendering
  results. It opens square shape (for
  \code{\link{LinearGradient}} objects) or a circle (for
  \code{\link{RadialGradient}} objects). This shape is
  colored using the gradient definition.
}
\details{
  The gradient is transformed to fit into the shape.
}

