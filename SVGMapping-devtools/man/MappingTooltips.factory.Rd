\name{MappingTooltips.factory}
\alias{MappingTooltips.factory}
\title{Mapping Tooltips Factory}
\usage{
  MappingTooltips.factory(data, targets, trans.function,
    trans.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

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
  a \code{\link{MappingTooltips}} object
}
\description{
  This function returns a \emph{default}
  \code{\link{MappingTooltips}} instance.
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
## one column named x. Row names are the circles identifiers.
dummy <- data.frame(x=c("This is Circle A",
                        "This is Circle B",
                        "This is Circle C",
                        "This is Circle D",
                        "This is Circle E",
                        "This is Circle F"),
                   row.names=circles)

## Now, let's create a MappingTooltips instance using the default factory
my.map <- MappingTooltips.factory(dummy)

## Then, we apply this mapping object to the template, and show
## it in the default browser.
## mapping(template,my.map)
## show(template)
}

