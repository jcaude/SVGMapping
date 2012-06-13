if ("package:SVGMapping" %in% search())
  detach(name="package:SVGMapping", unload=TRUE, force=TRUE)
library(SVGMapping)

library(ggplot2)
data(Chem97, package = "mlmRev")
data(Oats, package = "MEMSS")

cat("\nGRID LAYOUT FOR THE USER2012 CONFERENCE\n\n")

cat(">>> 1- Grid layout used for useR! 2012\n")

# that is the same as above, but with only one panel on the left..
blank <- SVG.factory(dims="a4", landscape=TRUE)
vgrid <- VarGrid.factory(cols=c(0,0.25,0.28,1.0),opacity=1.0,prefix="Grid")
layout(blank) <- vgrid

lgrid <- FixGrid.factory(nrows=3,opacity=1.0,prefix="leftGrid")
layout(blank,blank["Grid.grid.1.1"][[1]]) <- lgrid

show(blank) ## to display the grid..

cat(">>> 2- Map Pathways...\n")

# set stroke-opacity to 0 for the spacer column
blank["Grid.grid.1.2","style::stroke-opacity"] <- 0.0

# put main pathway map on the main cell
main.map <- SVG.factory(file="svg/leplat-uranium.main.svg")
merge.SVG(blank,blank["Grid.grid.1.3"][[1]], preserve.ratio=TRUE) <- main.map

# put complementary maps on the left bar..
left1.map <- SVG.factory(file="svg/leplat-uranium.left1.svg")
merge.SVG(blank,blank["leftGrid.grid.1.1"][[1]], preserve.ratio=TRUE) <- left1.map
left2.map <- SVG.factory(file="svg/leplat-uranium.left2.svg")
merge.SVG(blank,blank["leftGrid.grid.2.1"][[1]], preserve.ratio=TRUE) <- left2.map
left3.map <- SVG.factory(file="svg/leplat-uranium.left3.svg")
merge.SVG(blank,blank["leftGrid.grid.3.1"][[1]], preserve.ratio=TRUE) <- left3.map
show(blank)

cat(">>> 3- Plots....\n")
Sys.sleep(5)

# that is the same as above, but with only one panel on the left..
blank <- SVG.factory(dims="a4", landscape=TRUE)
vgrid <- VarGrid.factory(cols=c(0,0.25,0.28,1.0),opacity=0.0,prefix="Grid")
layout(blank) <- vgrid

lgrid <- FixGrid.factory(nrows=3,opacity=0.0,prefix="leftGrid")
layout(blank,blank["Grid.grid.1.1"][[1]]) <- lgrid

# Main cell
dev <- svgDevice(blank,blank["Grid.grid.1.3"][[1]],pointsize=12)
pg <- ggplot(Chem97, aes(gcsescore)) +
  stat_density(geom = "path",
               position = "identity", aes(colour = factor(score)))
print(pg)
dev.off()

# Left 1
dev <- svgDevice(blank,blank["leftGrid.grid.1.1"][[1]],pointsize=8)
p <- ggplot(faithful, aes(eruptions))
pg <- p + stat_density(geom = "path", position = "identity") +
  geom_point(aes(y = 0.05), position = position_jitter(height = 0.005), alpha = 0.25)
print(pg)
dev.off()

# Left 2
dev <- svgDevice(blank,blank["leftGrid.grid.2.1"][[1]],pointsize=8)
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

# Left 3
dev <- svgDevice(blank,blank["leftGrid.grid.3.1"][[1]],pointsize=8)
# Define cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Create a pie chart with defined heading and
# custom colors and labels
pie(cars, main="Cars", col=rainbow(length(cars)),
   labels=c("Mon","Tue","Wed","Thu","Fri"))
dev.off()

show(blank)
