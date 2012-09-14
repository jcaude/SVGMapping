\name{MappingLinks.factory}
\alias{MappingLinks.factory}
\title{Mapping Links Factory}
\usage{
  MappingLinks.factory(data, targets, url.pattern,
    trans.function, trans.parameters)
}
\arguments{
  \item{data}{is the input dataset to use for this mapping}

  \item{targets}{is the list of template node targets to
  alter. This can be a list of SVG nodes identifiers or any
  node selection expression. By default the targets are the
  row names of the input data variable.}

  \item{url.pattern}{contains the url pattern format string
  that is used to forge url from input values}

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
  a \code{\link{MappingLinks}} object
}
\description{
  This function returns a \code{\link{MappingLinks}}
  instance that can be used to associate hyperlinks to a
  set of \emph{targets} SVG elements. The links URL are
  forge using input values and if set the url pattern.
}
\examples{
## load 'basic-sample.svg' a demo SVG template.
## template <- SVG.factory(file=system.file("extdata/basic-sample.svg",package="SVGMapping))

## In this demo template, the top six circles are identified with the
## 'circle.A' ... 'circle.F' svg ID attributes. We will generate a list that
## contains such identifiers..
circles <- paste("circle.",LETTERS[1:6],sep="")

## Then, we will use the following dummy dataset. This data.frame contains
## one column named x. Row names are the circles identifiers. We will
## associate a link to the twitter accounts stored in the data-frame
dummy <- data.frame(x=c("BarackObama","MittRomney","NYTLive",
                        "Zidane_Zinedine","KingJames","IronmanTri"),
                   row.names=circles)

## Now, let's create a MappingLinks instance using the default factory
## function. We initialize the url pattern to point to twitter
my.map <- MappingLinks.factory(dummy[,"x",drop=FALSE])
urlPattern(my.map) <- "http://twitter.com/"

## Then, we apply this mapping object to the template, and show
## it in the default browser.
## mapping(template,my.map)
## show(template)
}

