if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")

cat("\nDUPLICATE.NODE TESTS\n\n")
cat(">>> 1- Duplicate 'circle' A...'\n")

circle.A <- svg["circle.A"][[1]]
circle.AA <- duplicate.node(svg,circle.A,"circle.")
circle.AA.attr <- xmlAttrs(circle.AA)
circle.AA.id <- circle.AA.attr["id"]
cat("- id of the duplicate node=",circle.AA.id,"\n")
addChildren(svg["circle.group"][[1]],kids=list(circle.AA))
show(svg)

cat(">>> 2- Move 'circle.A' to the end and make it red...'")
readline("\nType <Return>\t to continue : ")

svg[circle.AA.id,"style::fill"] <- "red"
AA.transform <- svg[circle.AA.id,"transform"]
AA.transform <- paste(AA.transform,"translate(460,0)",sep=" ")
svg[circle.AA.id,"transform"] <- AA.transform
show(svg)
