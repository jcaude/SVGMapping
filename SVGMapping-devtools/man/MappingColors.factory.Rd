\name{MappingColors.factory}
\alias{MappingColors.factory}
\title{Mapping Colors Factory}
\usage{
  MappingColors.factory(data, targets, target.attribute,
    map.colors, map.range, gradient.type, fill.angle, fn,
    fn.parameters)
}
\arguments{
  \item{data}{is the input datasets to use for this
  mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

  \item{target.attribute}{specifies to which attribute the
  color value will be set. It can be either an SVG
  attribute (eg. \emph{color}) or a CSS style sub-attribute
  (eg. \emph{style::fill} or \emph{style::stroke}).}

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

  \item{fn}{is the transformation function that is applied
  onto the data, prior to the color mapping (see the
  \code{\link{Mapping}} class documentation).  By default
  the \emph{identity} function, which do not transformed
  the input data, is assigned to the newly created
  instance.}

  \item{fn.parameters}{is the list of parameters values
  associated with the transformation function.}
}
\value{
  a \code{\link{MappingColors}} object
}
\description{
  This function returns a \code{\link{MappingColors}}
  instance that will change the filling and stroke colors
  of template targets according to input values.
}
\details{
  Once defined a \code{MappingColors} instance can be
  applied to the template using the \code{mapping}
  function.
}
\examples{
## load 'basic-sample.svg' a demo SVG template.
## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))

## In this demo template, the top six circles are identified with the
## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
## contains such identifiers..
circles <- paste("circle.",LETTERS[1:6],sep="")

## Then, we will use the following dummy dataset. This data.frame contains
## three columns named x,y and z. Row names are the circles identifiers.
dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
                   y=rep(3,times=6),
                   z=runif(6,min=-5,max=5),
                   row.names=circles)

## ----- MappingColors example 1
## First, let's create a MappingColors instance using the default factory
## function. Then, we apply this mapping object to the template, and show
## it in the default browser.
color.map <- MappingColors.factory(dummy[,"x",drop=FALSE])
## mapping(template,color.map)
## show(template)

## ----- MappingColors example 2
## This time we will create a new instance with different colors, and use
## a dedicated transformation function to simulate a three states filter.
## data values inferior to 2 are colored in red, in the range [2,4] in
## orange and above 4 in green.

## dummy data
dummy2 <- data.frame(state=runif(6,min=0,max=5),
                   row.names=circles)

## colors vector
cust.colors <- c("#FF0000","#FFA500","#0000FF")

## user transformation function
cut.fn <- function(x,p) {return(ifelse(x<2,0,ifelse(x>4,2,1)))}

## create & init. the mapping color operation
traffic.map <- MappingColors.factory(dummy2[,"state",drop=FALSE])
mapColors(traffic.map) <- cust.colors
mapRange(traffic.map) <- c(0,2)
fnUser(traffic.map,cut.fn,list())

## apply & show mapping results
## mapping(template,traffic.map)
## show(template)
}

