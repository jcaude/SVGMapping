\name{MappingOpacity.factory}
\alias{MappingFillOpacity.factory}
\alias{MappingOpacity.factory}
\alias{MappingStrokeOpacity.factory}
\title{Mapping Opacity Factory}
\usage{
  MappingOpacity.factory(data, targets, fn, fn.parameters)

  MappingFillOpacity.factory(data, targets, fn,
    fn.parameters)

  MappingStrokeOpacity.factory(data, targets, fn,
    fn.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

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
  This function returns a \code{\link{MappingValues}}
  instance that can be use to change the opacity level
  according to some input values.
}
\details{
  By default, this factory function expect that input
  values are levels in the range [0,1]. Thus, if inputs
  does not fulfill this constraint one can provide a
  transformation function to scale the input data.

  The \code{MappingOpacity.factory} factory function will
  create a mapping operation that change the
  \strong{overall} opacity of the target shapes.

  The \code{MappingFillOpacity.factory} factory function
  will create a mapping operation that only change the
  \strong{filling} opacity of the target shapes.

  The \code{MappingStrokeOpacity.factory} factory function
  will create a mapping operation that only change the
  \strong{stroke} opacity of the target shapes.
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

## -------------- Overall Opacity Example
## Now, let's create a MappingValues instance using the default factory
## function. We initialize the target attribute to 'opacity'
all.map <- MappingOpacity.factory(dummy[,"x",drop=FALSE])

## Then, we apply this mapping object to the template, and show
## it in the default browser.
## mapping(template, all.map)
## show(template)

## -------------- Filling Opacity Example
## just using a different factory function we can restrict the change
## of opacity to only the filling of shapes
fill.map <- MappingFillOpacity.factory(dummy[,"x",drop=FALSE])
## mapping(template, fill.map)
## show(template)

## -------------- Stroke Opacity Example
## and now the stroke opacity..
stroke.map <- MappingStrokeOpacity.factory(dummy[,"x",drop=FALSE])
## mapping(template, stroke.map)
## show(template)
}
\seealso{
  Other possible factory functions are:
  \code{\link{MappingValues.factory}},
  \code{\link{MappingFillOpacity.factory}},
  \code{\link{MappingStrokeOpacity.factory}} and
  \code{\link{MappingStrokeWidth.factory}}
}

