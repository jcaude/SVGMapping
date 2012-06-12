if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)


cat("\nGRID LAYOUT TESTS\n\n")
cat(">>> 1- Create a fixed grid (3x3)\n")

grid <- FixGrid.factory("myGrid",3,3,opacity=1)
print(grid)
show(grid)

cat(">>> 2- Add a fixed grid (2x3) on a blank A4 document\n")
Sys.sleep(3)

blank <- SVG.factory(dims="a4", landscape=TRUE)
grid <- FixGrid.factory("myGrid",2,3,opacity=1)
layout(blank) <- grid
show(blank)

cat(">>> 3- Add a sub-grid (2x2) in the upper-center cell\n")
Sys.sleep(3)

subgrid <- FixGrid.factory("subGrid",2,2,opacity=0.9)
layout(blank,blank["myGrid.grid.1.2"][[1]]) <- subgrid
show(blank)

cat(">>> 4- Put random plots & cows in the grid\n")
Sys.sleep(3)

dev <- svgDevice(blank,blank["myGrid.grid.2.2"][[1]],pointsize=8)
plot(density(rnorm(100)),main="random normal density")
dev.off()

cows <- SVG.factory("svg/openclipart_cows.svg")
merge.SVG(blank,blank["myGrid.grid.1.3"][[1]]) <- cows

for(i in 1:2) {
  for(j in 1:2) {
    id <- paste("subGrid.grid.",i,".",j,sep="")
    dev <- svgDevice(blank,blank[id][[1]],pointsize=6)
    plot(density(rnorm(20,sd=5)))
    dev.off()
  }
}

apple <- SVG.factory(system.file("extdata/gnokii-Apple2.svg",package="SVGMapping"))
merge.SVG(blank,blank["myGrid.grid.2.1"][[1]],preserve.ratio=TRUE) <- apple

dev <- svgDevice(blank,blank["myGrid.grid.1.1"][[1]],pointsize=8)
# Get a random log-normal distribution
r <- rlnorm(1000)

# Get the distribution without plotting it using tighter breaks
h <- hist(r, plot=F, breaks=c(seq(0,max(r)+1, .1)))

# Plot the distribution using log scale on both axes, and use
# blue points
plot(h$counts, log="xy", pch=20, col="blue",
	main="Log-normal distribution",
	xlab="Value", ylab="Frequency")
dev.off()

dev <- svgDevice(blank,blank["myGrid.grid.2.3"][[1]],pointsize=8)
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Create a pie chart with defined heading and
# custom colors and labels
pie(cars, main="Cars", col=rainbow(length(cars)),
   labels=c("Mon","Tue","Wed","Thu","Fri"))
dev.off()
show(blank)

cat(">>> 5- And now same idea with variable grid layout\n")
Sys.sleep(5)

blank <- SVG.factory(dims="a4", landscape=TRUE)
vgrid <- VarGrid.factory(cols=c(0,0.2,0.8,1.0),opacity=1.0,prefix="Grid")
layout(blank) <- vgrid

lgrid <- FixGrid.factory(nrows=3,opacity=1.0,prefix="leftGrid")
rgrid <- FixGrid.factory(nrows=3,opacity=1.0,prefix="rightGrid")
layout(blank,blank["Grid.grid.1.1"][[1]]) <- lgrid
layout(blank,blank["Grid.grid.1.3"][[1]]) <- rgrid


dev <- svgDevice(blank,blank["Grid.grid.1.2"][[1]],pointsize=12)
# Get a random log-normal distribution
r <- rlnorm(1000)

# Get the distribution without plotting it using tighter breaks
h <- hist(r, plot=F, breaks=c(seq(0,max(r)+1, .1)))

# Plot the distribution using log scale on both axes, and use
# blue points
plot(h$counts, log="xy", pch=20, col="blue",
	main="Log-normal distribution",
	xlab="Value", ylab="Frequency")
dev.off()

for(i in 1:3) {
  id <- paste("leftGrid.grid.",i,".1",sep="")
  dev <- svgDevice(blank,blank[id][[1]],pointsize=6)
  plot(density(rnorm(20,sd=5)))
  dev.off()
}

cows <- SVG.factory("svg/openclipart_cows.svg")
merge.SVG(blank,blank["rightGrid.grid.1.1"][[1]],preserve.ratio=TRUE) <- cows

apple <- SVG.factory(system.file("extdata/gnokii-Apple2.svg",package="SVGMapping"))
merge.SVG(blank,blank["rightGrid.grid.3.1"][[1]],preserve.ratio=TRUE) <- apple

dev <- svgDevice(blank,blank["rightGrid.grid.2.1"][[1]],pointsize=8)
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Create a pie chart with defined heading and
# custom colors and labels
pie(cars, main="Cars", col=rainbow(length(cars)),
   labels=c("Mon","Tue","Wed","Thu","Fri"))
dev.off()
show(blank)
