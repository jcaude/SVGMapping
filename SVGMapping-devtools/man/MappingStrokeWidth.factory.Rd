\name{MappingStrokeWidth.factory}
\alias{MappingStrokeWidth.factory}
\title{Mapping Stroke Width Factory}
\usage{
  MappingStrokeWidth.factory(data, targets, fn,
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
  instance that can be use to change the stroke width
  according to some input values.
}
\details{
  By default, this factory function expect that input
  values are numerics given in the SVG user coordinates
  system. Thus, if inputs are not given in this system on
  can fix this by using the \code{\link{targetUnit}}
  method.
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
dummy <- data.frame(x=runif(6,min=1,max=5),
                   row.names=circles)

## Now, let's create a MappingValues instance using the stroke width factory
## function. We also set the target attribute unit to 'cm'
my.map <- MappingOpacity.factory(dummy[,"x",drop=FALSE])
targetUnit(my.map) <- "cm"

## Then, we apply this mapping object to the template, and show
## it in the default browser.
## mapping(template, my.map)
## show(template)
}

