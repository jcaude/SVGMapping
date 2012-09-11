\name{MappingValues.factory}
\alias{MappingValues.factory}
\title{Mapping Values Factory}
\usage{
  MappingValues.factory(data, targets, target.attribute,
    target.unit, fn, fn.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

  \item{target.attribute}{specifies to which attribute the
  output value will be set. It can be either an SVG
  attribute (eg. \emph{opacity}) or a CSS style
  sub-attribute (eg. \emph{style::stroke} or
  \emph{style::x}).}

  \item{target.unit}{contains the unit(s) character
  string(s) that are use as postfix for output values.}

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
  a \code{\link{MappingValues}} object
}
\description{
  This function returns a \emph{default}
  \code{\link{MappingValues}} instance.
}
\details{
  Using this factory function, the return object has no
  target attribute and no transformation function. Only the
  input \emph{values} are initialized in the object. Thus,
  by default this mapping operation will have no effect if
  applied to an SVG template (unless one has use the
  relevant methods to change this operation behavior).
}
\examples{
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

## Now, let's create a MappingValues instance using the default factory
## function. We initialize the target attribute to 'opacity'
my.map <- MappingValues.factory(dummy[,"x",drop=FALSE])
targetAttribute <- "opacity"

## Then, we apply this mapping object to the template, and show
## it in the default browser.
## mapping(template,my.map)
## show(template)
}
\seealso{
  Other possible factory functions are:
  \code{\link{MappingOpacity.factory}},
  \code{\link{MappingFillOpacity.factory}},
  \code{\link{MappingStrokeOpacity.factory}} and
  \code{\link{MappingStrokeWidth.factory}}
}

