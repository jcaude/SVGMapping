if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

jsAnimation(svg) <- TRUE  ## to force js script initialization

print(svg)
show(svg)
cat("\nAfter script addition\n")
print(svg)
