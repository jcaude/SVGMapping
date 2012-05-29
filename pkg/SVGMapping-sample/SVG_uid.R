if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")
circle.ids <- paste("circle.",LETTERS[1:6],sep="")

cat("\nUID TESTS\n\n")
cat(">>> 1- Get UID for a new blue 'circle'\n")
circle.uid <- uid(svg,prefix="circle.",n=1)
cat("- Circle UID=",circle.uid,"\n")
new.circle <- newXMLNode("circle",
                         attrs=list(
                           id=circle.uid,
                           cx="583.96283",
                           cy="464.41602",
                           r="16.730175",
                           style="fill:none;stroke:#0000FF;stroke-width:2"
                           )
                         )
addChildren(svg["circle.group"][[1]],kids=list(new.circle))
show(svg)

cat(">>> 2- Restore circle IDs'\n")

cids <- svg[c("xpath:://svg:path[starts-with(@id,'circle')]","xpath:://circle[starts-with(@id,'circle')]"),"id"]
cat("- IDs=",paste(unlist(cids),collapse=", "),"\n")



