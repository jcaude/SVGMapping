if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

svg <- SVG.factory(file="svg/sample0.svg")
circle.ids <- paste("circle.",LETTERS[1:6],sep="")

cat("\nSETTER TESTS\n\n")

cat(">>> 0- View original SVG'")
readline("\nType <Return>\t to start : ")

show(svg)

cat(">>> 1- Hide circle labels'")
readline("\nType <Return>\t to continue : ")

svg["circle.group.label","opacity"] <- 0.0
show(svg)

cat(">>> 2- Restore circle labels'")
readline("\nType <Return>\t to continue : ")

svg["circle.group.label","opacity"] <- 1.0
show(svg)

cat(">>> 3- Increasing circle stroke width and make it blue'")
readline("\nType <Return>\t to continue : ")

circle.ids <- paste("circle.",LETTERS[1:6],sep="")
svg[circle.ids,"style::stroke"] <- '#0000FF'
svg[circle.ids,"style::stroke-width"] <- (1:6)*0.66
show(svg)

cat(">>> 4- Change Fill color (greyscale)'")
readline("\nType <Return>\t to continue : ")

for(i in 1:6)
  svg[paste("circle.",LETTERS[i],sep=""),"style::fill"] <- paste("#",paste(rep(LETTERS[i],6),collapse=""),sep="")
show(svg)

cat(">>> 5- Change label colors (randomly)'")
readline("\nType <Return>\t to continue : ")

for(i in 1:6) {
  color_idx <- round(runif(min=0,max=15,n=6))
  colors <- as.character(color_idx)
  if(max(color_idx) >= 10) 
    colors[color_idx >= 10] <- LETTERS[color_idx[color_idx >= 10]-9]
  rgb <- paste("#",paste(colors,collapse=""),sep="")
  svg[paste("circle.",LETTERS[i],".label",sep=""),"style::fill"] <- rgb
}
show(svg)
