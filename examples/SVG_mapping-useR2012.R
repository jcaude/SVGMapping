if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

circle.ids <- paste("circle.",LETTERS[1:6],sep="")

data <- data.frame(x=rep(3,times=6),
                   y=runif(6,min=0.5,max=1))

fill.map <- MappingFillColors.factory(data[,"x"], targets=circle.ids, 
                                  fn="Random", fn.parameters=list(min=-2,max=2))

stroke.map <- MappingStrokeColors.factory(data[,"x"], targets=circle.ids, 
                                          fn="Random", fn.parameters=list(min=-2,max=2))

Swidth.map <- MappingStrokeWidth.factory(data[,"x"], targets=circle.ids)

Sopac.map <- MappingStrokeOpacity.factory(data[,"y"], targets=circle.ids)

mapping(svg,fill.map)
mapping(svg,stroke.map)
mapping(svg,Swidth.map)
mapping(svg,Sopac.map)

show(svg)


