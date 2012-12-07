\name{MappingStrokeColors.factory}
\alias{MappingStrokeColors.factory}
\title{Mapping Stroke Colors Factory}
\usage{
  MappingStrokeColors.factory(data, targets, map.colors,
    map.range, gradient.type, fill.angle, trans.function,
    trans.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

  \item{map.colors}{to use by the mapping operations. The
  default value is a color map that runs from blue to red,
  with 100 intermediates levels. This color map has been
  generated using the \code{blue2red} function from the
  \strong{colorRamps} package.}

  \item{map.range}{is a vector that contains the bounds of
  the linear transformation used to convert numerical
  values into colors. The default range value is
  \eqn{[0,1]}{[0,1]}. Outliers are converted to the closest
  bound of this range. For more details about map ranges
  see the \code{\link{mapRange}} method.}

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
  created instance.}

  \item{trans.parameters}{is the list of parameters values
  associated with the transformation function.}
}
\value{
  a \code{\link{MappingColors}} object
}
\description{
  This function returns a \code{\link{MappingColors}}
  instance that will change the \strong{stroke} colors of
  template targets according to input values.
}
\details{
  For more details about Mapping Colors factory function
  see the \code{\link{MappingColors.factory}} function
  documentation.
}
\examples{
## see also the manpage of MappingColors.factory for additional examples.

## load 'basic-sample.svg' a demo SVG template.
## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))

## In this demo template, the top six circles are identified with the
## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
## contains such identifiers..
circles <- paste("circle.",LETTERS[1:6],sep="")

## Then, we will use the following dummy dataset. This data.frame contains
## one column named x. Row names are the circles identifiers.
dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
                   row.names=circles)

## ----- Mapping Stroke Colors example
## First, let's create a MappingColors instance using the factory
## function. Then, we apply this mapping object to the template, and show
## it in the default browser.
color.map <- MappingStrokeColors.factory(dummy[,"x",drop=FALSE])
## mapping(template,color.map)
## show(template)
}
\seealso{
  Other possible factory functions are:
  \code{\link{MappingColors.factory}},
  \code{\link{MappingFillColors.factory}} and
  \code{\link{MappingBioArraysColors.factory}}
}

