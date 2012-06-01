if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

cat("\nsvgDEVICE TESTS\n\n")

cat(">>> 1- Plot the density of 'rnorm(1000)' in grid.1 rectangle'\n")
svgDevice(svg,svg["grid.1"][[1]],pointsize=8)
plot(density(rnorm(1000)))
dev.off()
svg["grid.1.label","opacity"] <- 0.0
show(svg)
