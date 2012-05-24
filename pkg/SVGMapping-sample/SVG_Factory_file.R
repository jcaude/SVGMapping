if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

print(svg)
