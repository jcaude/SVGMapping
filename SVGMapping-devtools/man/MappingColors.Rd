\name{MappingColors}
\alias{MappingColors}
\alias{MappingColors-class}
\title{Colors Mapping Operation}
\description{
  This class allows to convert numerical values into colors
  and use them to alter the target shapes. For example,
  this class can be use to change the fill or the stroke
  colors of shapes.
}
\details{
  The conversion between \emph{values} and \emph{colors}
  use a linear mapping.  To perform this operation we make
  several assumptions. First, we consider that
  \emph{values} are in the range
  [\code{maprange.min},\code{maprange.max}], if
  \emph{values} are rounded to these bounds. Second,
  \emph{colors} must be given as a list using the following
  string format: \code{#RRGGBB}. Giving many intermediate
  colors will usually result in smoother rendering. At this
  point we mention that a linear mapping is performed for
  the \emph{values} to \emph{colors} mapping. If one wants
  a different mapping function, he must use a
  \emph{transformation} function. This function must
  convert \emph{value} to a linear scale that will be
  suitable for the color mapping operation.

  The MappingColors operation can be operate using either
  \emph{univariate} dataset (ie. one numerical value for
  each \emph{target}) or \emph{multivariate} (ie. at leat
  two values per \emph{target}) datasets. In the univariate
  case the the single numerical value is converted into an
  SVG color specification that is applied to the specified
  attribute. In the multivariate case, each numerical value
  is converted into a color specification using the same
  process previously described. Then, this set is converted
  into a linear or radial gradient with a given angle (as
  specified in the mapping colors instance).
}

