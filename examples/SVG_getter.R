if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")
circle.ids <- paste("circle.",LETTERS[1:6],sep="")

cat("\nGETTER TESTS\n\n")
cat(">>> 1- Get 'circles node'\n")
circle.nodes <- svg["xpath:://*[starts-with(@id,'circle')]"]
str(circle.nodes)
cat(">>> 2- Get ALL 'circle' IDs\n")
ids <- svg["xpath:://*[starts-with(@id,'circle')]","id"]
cat(paste(ids,sep=", "),"\n")
cat(">>> 3- Get 'circle (A-F)' current syle\n") 
circle.style <- svg[circle.ids, "style"]
cat(circle.style[[1]],"\n")
cat(">>> 4- Get 'circle (A-F)' stroke\n") 
circle.style <- svg[circle.ids, "style::stroke"]
cat(circle.style[[1]],"\n")
