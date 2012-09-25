\name{MappingMasks.factory}
\alias{MappingMasks.factory}
\title{Mapping Mask Factory}
\usage{
  MappingMasks.factory(data, targets, fill.angle,
    trans.function, trans.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

  \item{fill.angle}{is the filling angle of the gradient.
  The default value for this parameter is -pi/2.}

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
  a \code{\link{MappingMasks}} object
}
\description{
  This function returns a \emph{default}
  \code{\link{MappingMasks}} instance.
}
\details{
  Using this factory function, input \emph{values} and
  \emph{targets} are initialized in the object. This
  mapping operation associate default tooltips to the
  targets shapes if applied to an SVG template. The tooltip
  is very simple, with only one line that is filled using
  input \emph{values}. Thus, the input value data.frame
  must only contain one column of character strings.
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
                   row.names=circles)

## First, we color all circles in blue using SVG accessors
## template[circles,"fill:color"] <- "blue"

## Then, we create a MappingMask instance using the factory
## function. Then, we apply this mapping object to the template, and show
## it in the default browser.
mask.map <- MappingMasks.factory(dummy[,"x",drop=FALSE])
## mapping(template,color.map)
## show(template)
}

