\name{MappingBioArraysColors.factory}
\alias{MappingBioArraysColors.factory}
\title{Mapping Expression Arrays Colors Factory}
\usage{
  MappingBioArraysColors.factory(arrays, targets,
    map.colors, map.range, gradient.type, fill.angle,
    trans.function, trans.parameters)
}
\arguments{
  \item{arrays}{contains expression fold-change values}

  \item{targets}{is the list of template node targets to
  alter. Depending on the template, it can be a list of
  genes/proteins names. By default the targets are the row
  names of the input arrays.}

  \item{map.colors}{to use by the mapping operations. The
  default value is a color map that runs from green to red,
  with blank in the middle, in 1,000 intermediates levels.}

  \item{map.range}{is a vector that contains the bounds of
  the linear transformation used to convert numerical
  values into colors. The default range value is
  \eqn{[-2,2]}{[-2,2]}. Outliers are converted to the
  closest bound of this range. For more details about map
  ranges see the \code{\link{mapRange}} method.}

  \item{gradient.type}{defines the kind of gradient
  (\emph{linear} or \emph{radial}) to use when dealing with
  multivariate input datasets. By default a \emph{linear}
  gradient is selected.}

  \item{fill.angle}{is the filling angle of the gradient.
  The default value for this parameter is 0.}

  \item{trans.function}{is the transformation function that
  is applied onto the data, prior to the color mapping (see
  the \code{\link{Mapping}} class documentation). By
  default the \emph{identity} function, which do not
  transformed the input data, is assigned to the newly
  created instance. If input arrays are \emph{log-ratios},
  one can easily convert them to \emph{fold-change} using
  the \code{\link{fnLog2FC}} function.}

  \item{trans.parameters}{is the list of parameters values
  associated with the transformation function.}
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
\examples{
## see also the manpage of MappingColors.factory for additional examples.

## load 'basic-sample.svg' a demo SVG template.
## -- we will consider that circles are genes/proteins symbols
## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))

## In this demo template, the top six circles are identified with the
## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
## contains such identifiers..
circles <- paste("circle.",LETTERS[1:6],sep="")

## Then, we will use the following dummy dataset. This data.frame contains
## one column FC which contains fold-change values. Row names are the circles
## identifiers.
dummy <- data.frame(FC=c(0.1,-1.2,1.4,0.6,-0.8,-1.0),
                   row.names=circles)

## ----- Mapping Stroke Colors example
## First, let's create a MappingBioArraysColors instance using the factory
## function. Then, we apply this mapping object to the template, and show
## it in the default browser.
FC.map <- MappingBioArraysColors.factory(arrays=dummy[,"x",drop=FALSE])
## mapping(template,FC.map)
## show(template)
}
\seealso{
  Other possible factory functions are:
  \code{\link{MappingColors.factory}},
  \code{\link{MappingFillColors.factory}} and
  \code{\link{MappingStrokeColors.factory}}
}

