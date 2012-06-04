if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)


cat("\nGRID LAYOUT TESTS\n\n")
cat(">>> 1- Create a fixed grid (3x3)\n")

grid <- FixGrid.factory("myGrid",3,3,opacity=1)
print(grid)
