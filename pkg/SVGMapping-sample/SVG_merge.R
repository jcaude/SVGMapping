if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

cat("\nDUPLICATE.NODE TESTS\n\n")

cat(">>> 1- Merge 'cows' in grid.1 rectangle'")
readline("\nType <Return>\t to start : ")

cows <- SVG.factory(file="svg/openclipart_cows.svg")
merge.SVG(svg,svg["grid.1"][[1]]) <- cows
svg["grid.1.label","opacity"] <- 0.0
show(svg)

cat(">>> 2- Merge 'cows' (preserve ratio) in grid.2 rectangle\n")
Sys.sleep(3)

cows <- SVG.factory(file="svg/openclipart_cows.svg")
merge.SVG(svg,svg["grid.2"][[1]],preserve.ratio=TRUE) <- cows
svg["grid.2.label","opacity"] <- 0.0
show(svg)

cat(">>> 3- Merge 'sample0' (itself) in grid.3 rectangle\n")
Sys.sleep(3)

sample0 <- SVG.factory(file="svg/sample0.svg")
merge.SVG(svg,svg["grid.3"][[1]]) <- sample0
svg["grid.3.label","opacity"] <- 0.0
show(svg)

cat(">>> 4- Merge 'sample0' (itself+pres.ratio) in grid.4 rectangle\n")
Sys.sleep(3)

sample0 <- SVG.factory(file="svg/sample0.svg")
merge.SVG(svg,svg["grid.4"][[1]],preserve.ratio=TRUE,prefix="sample0.") <- sample0
svg["grid.4.label","opacity"] <- 0.0
show(svg)

cat(">>> 5- Merge 'cows' on the whole document\n")
Sys.sleep(3)

cows <- SVG.factory(file="svg/openclipart_cows.svg")
merge.SVG(svg,preserve.ratio=TRUE) <- cows
show(svg)

cat(">>> 6- Merge 'cows' in the sub-grid.2 of grid.4\n")
Sys.sleep(3)

cows <- SVG.factory(file="svg/openclipart_cows.svg")
merge.SVG(svg,svg["sample0.grid.2"][[1]]) <- cows
svg["sample0.grid.2.label","opacity"] <- 0.0
show(svg)

