if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)


cat("\nSHAPES TESTS\n\n")
cat(">>> 1- Create a 'rectangle' shape\n")

rect <- SVGRect.factory(coords=list(x=10,y=10,width=100,height=100),style="fill:red")
print(rect)

cat("\n>>> 2- Create a 'line' shape\n")

line <- SVGLine.factory(x1=10,y1=10,x2=100,y2=100,style="stoke:blue")
print(line)
