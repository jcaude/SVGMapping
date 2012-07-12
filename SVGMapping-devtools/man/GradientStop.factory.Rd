\name{GradientStop.factory}
\alias{GradientStop.factory}
\title{GradientStop Factory}
\usage{
  GradientStop.factory(offset = "0", color = "white",
    opacity = 1)
}
\arguments{
  \item{offset}{is string defining the \emph{stop} offset
  value (see SVG 1.1 specification)}

  \item{color}{is a string defining the color of the
  \emph{stop} (see SVG 1.1 specification)}

  \item{opacity}{is a numeric value in the range [0,1]
  defining the opacity level of the \emph{stop}}
}
\value{
  a \code{\link{GradientStop}} instance
}
\description{
  This function build a Gradient Stop instance given an
  offset, a color and an opacity level
}
\details{
  The default values are:

  \itemize{ \item \code{offset = '0'} \item \code{color =
  'white'} \item \code{opacity = 1.0} }
}
\examples{
mystop <- GradientStop.factory(offset="0.5",color="#AAFFBB", opacity=0.90)
}
\seealso{
  The \code{\link{GradientStop}} class definition
}

