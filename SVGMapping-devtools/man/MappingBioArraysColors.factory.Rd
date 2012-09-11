\name{MappingBioArraysColors.factory}
\alias{MappingBioArraysColors.factory}
\title{Mapping Expression Arrays Colors Factory}
\usage{
  MappingBioArraysColors.factory(arrays)
}
\arguments{
  \item{arrays}{contains expression fold-change values}
}
\value{
  a \code{\link{MappingColors}} object
}
\description{
  This function returns a \code{\link{MappingColors}}
  instance that will change the \strong{filling} colors of
  template targets according to some expression fold-change
  values, usually measured using microarrays. This function
  uses some custom parameters that are dedicated to
  bioinformatics analysis.
}
\details{
  By default filling colors are taken from a green-red
  gradient with a white middle color. Here, green colors
  are associated with down-regulated genes and red colors
  with up-regulated genes. If input data are vectors
  (\emph{ie} multiple conditions or kinetic experiments),
  then a linear gradient will be generated.

  Expression values are mapped to colors in the range
  [-2,2]. One can easily modify these bounds using the
  \code{\link{mapRange}} method. Furthermore, these values
  are expected to be fold-changes. If not, one can use the
  dedicate \code{\link{fnLog2FC}} transformation method for
  base 2 log-ratios conversion or any user defined
  transformation function (see the \code{\link{fnUser}}
  methods).
}
\seealso{
  Other possible factory functions are:
  \code{\link{MappingColors.factory}},
  \code{\link{MappingFillColors.factory}} and
  \code{\link{MappingStrokeColors.factory}}
}

