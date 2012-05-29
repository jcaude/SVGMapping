if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

cat("\nDUPLICATE.NODE TESTS\n\n")
cat(">>> 1- Merge 'cows' in grid.1 rectangle'\n")

cows <- SVG.factory(file="svg/openclipart_cows.svg")
#trace("merge.SVG<-", browser, exit=browser, signature = c("SVG"))
merge.SVG(svg,svg["grid.1"][[1]]) <- cows
#show(svg)
