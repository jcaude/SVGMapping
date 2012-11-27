require(testthat)

.default_colors <- c("#0000FF", "#0005FF", "#000AFF", "#000FFF", 
                     "#0014FF", "#001AFF", "#001FFF", "#0024FF", 
                     "#0029FF", "#002EFF", "#0033FF", "#0038FF",  
                     "#003DFF", "#0042FF", "#0047FF", "#004DFF", 
                     "#0052FF", "#0057FF", "#005CFF", "#0061FF", 
                     "#0066FF", "#006BFF", "#0070FF", "#0075FF",  
                     "#007AFF", "#0080FF", "#0085FF", "#008AFF", 
                     "#008FFF", "#0094FF", "#0099FF", "#009EFF", 
                     "#00A3FF", "#00A8FF", "#00ADFF", "#00B3FF",  
                     "#00B8FF", "#00BDFF", "#00C2FF", "#00C7FF",
                     "#00CCFF", "#00D1FF", "#00D6FF", "#00DBFF",
                     "#00E0FF", "#00E6FF", "#00EBFF", "#00F0FF", 
                     "#00F5FF", "#00FAFF", "#00FFFF", "#FFFA00", 
                     "#FFF500", "#FFEF00", "#FFEA00", "#FFE500",
                     "#FFE000", "#FFDB00", "#FFD500", "#FFD000", 
                     "#FFCB00", "#FFC600", "#FFC100", "#FFBB00",
                     "#FFB600", "#FFB100", "#FFAC00", "#FFA700",
                     "#FFA100", "#FF9C00", "#FF9700", "#FF9200", 
                     "#FF8D00", "#FF8700", "#FF8200", "#FF7D00",
                     "#FF7800", "#FF7200", "#FF6D00", "#FF6800",
                     "#FF6300", "#FF5E00", "#FF5800", "#FF5300",  
                     "#FF4E00", "#FF4900", "#FF4400", "#FF3E00", 
                     "#FF3900", "#FF3400", "#FF2F00", "#FF2A00",
                     "#FF2400", "#FF1F00", "#FF1A00", "#FF1500",  
                     "#FF1000", "#FF0A00", "#FF0500", "#FF0000")

context("Mapping (Colors)")

test_that("get/set Fields", {
  
  ## standard constructor (empty object)
  ## -- we also check include fields of the 'Mapping' sub-class
  mapping <- new("MappingColors")
  
  expect_equal(targets(mapping),NULL)
  expect_equal(values(mapping),NULL)
  expect_equal(transFunction(mapping),NULL)
  expect_equal(transParameters(mapping),list())
  expect_equal(animations(mapping),FALSE)
  expect_equal(targetAttribute(mapping),character())
  expect_equal(mapColors(mapping),.default_colors)
  expect_equal(mapRange.min(mapping),0)
  expect_equal(mapRange.max(mapping),1)
  expect_equal(mapRange(mapping),c(0,1))
  expect_equal(gradientType(mapping),"linear")
  expect_equal(fillAngle(mapping),0)
  expect_is(mapping, "Mapping")
  expect_is(mapping, "MappingColors")
  
  ## initialize some specs
  targets(mapping) <- "xpath:://rect[@id='foobar']"
  values(mapping) <- c(1,2,3)
  transFunction(mapping) <- function(x,p) { return(x+p$c) }
  transParameters(mapping) <- list(c=2)
  animations(mapping) <- TRUE
  targetAttribute(mapping) <- "style::stroke"
  mapColors(mapping) <- c("0000FF","#FFFFFF","#FF0000")
  mapRange(mapping) <- c(-5,5)
  gradientType(mapping) <- "radial"
  fillAngle(mapping) <- 75
  
  ## check with new specs
  expect_equal(targets(mapping),"xpath:://rect[@id='foobar']")
  expect_equal(values(mapping),c(1,2,3))
  expect_true(is.function(transFunction(mapping)))
  expect_equal(gsub("[ \t]*", "",deparse(transFunction(mapping))),c("function(x,p)","{","return(x+p$c)","}"))
  expect_equal(transParameters(mapping),list(c=2))
  expect_equal(animations(mapping),TRUE)
  expect_equal(targetAttribute(mapping),"style::stroke")
  expect_equal(mapColors(mapping),c("0000FF","#FFFFFF","#FF0000"))
  expect_equal(mapRange.min(mapping),-5)
  expect_equal(mapRange.max(mapping),5)
  expect_equal(mapRange(mapping),c(-5,5))
  expect_equal(gradientType(mapping),"radial")
  expect_equal(fillAngle(mapping),75)
  
  ## fool accessors
  expect_error(targets(mapping) <- 2)
  expect_error(transFunction(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- list(a=2,"it must failed"))
  expect_error(animations(mapping) <- "it must failed")
  expect_error(targetAttribute(mapping) <- 15)
  expect_error(mapColors(mapping) <- matrix("#112233"))
  expect_error(mapRange.min(mapping) <- "0")
  expect_error(mapRange.max(mapping) <- "10")
  expect_error(mapRange(mapping) <- 10)
  expect_error(mapRange(mapping) <- c(1,1,1))
  expect_error(gradientType(mapping) <- "oval")
  expect_error(fillAngle(mapping) <- "180rad")
})

test_that("Factory Functions", {
  
  ## init.
  circles <- paste("circle.",LETTERS[1:6],sep="")
  dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
                      y=rep(3,times=6),
                      z=runif(6,min=-5,max=5),
                      row.names=circles)
  
  ## (default) MappingColors factory
  map <- MappingColors.factory(data=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::fill","style::stroke"))
  expect_equal(mapColors(map),.default_colors)
  expect_equal(mapRange(map),c(0,1))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),0)
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())

  ## (full args) MappingColors factory
  map <- MappingColors.factory(data=dummy[,"y"],
                               targets=circles,
                               target.attribute=c("style::stroke"),
                               map.colors=c("#00FF00", "#1CE300", "#39C600", 
                                            "#55AA00", "#718E00", "#8E7100",  
                                            "#AA5500", "#C63900", "#E31C00", 
                                            "#FF0000"),
                               map.range=c(-3.5,6.2),
                               gradient.type="radial",
                               fill.angle=182,
                               trans.function=fnRandom,
                               trans.parameters=list(min=-3.9,max=6.6))
  expect_equal(values(map),dummy[,"y"])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::stroke"))
  expect_equal(mapColors(map), c("#00FF00", "#1CE300", "#39C600", 
                                 "#55AA00", "#718E00", "#8E7100",  
                                 "#AA5500", "#C63900", "#E31C00", 
                                 "#FF0000"))
  expect_equal(mapRange.min(map),-3.5)
  expect_equal(mapRange.max(map),6.2)
  expect_equal(gradientType(map),"radial")
  expect_equal(fillAngle(map),182)
  expect_equal(transFunction(map),fnRandom)
  expect_equal(transParameters(map),list(min=-3.9,max=6.6))
  
  ## (default) MappingFillColors factory
  map <- MappingFillColors.factory(data=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::fill"))
  expect_equal(mapColors(map),.default_colors)
  expect_equal(mapRange(map),c(0,1))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),0)
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())
  
  ## (full args) MappingFillColors factory
  map <- MappingFillColors.factory(data=dummy[,"y"],
                                   targets=circles,
                                   map.colors=c("#4D4D4D", "#6C6C6C", "#838383", 
                                                "#969696", "#A7A7A7", "#B5B5B5",  
                                                "#C3C3C3", "#CFCFCF", "#DBDBDB", 
                                                "#E6E6E6"),
                                   map.range=c(-7,8),
                                   gradient.type="linear",
                                   fill.angle=25,
                                   trans.function=fnRangeLinear,
                                   trans.parameters=list(a=1,b=0,min=-5,max=5))
  expect_equal(values(map),dummy[,"y"])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::fill")
  expect_equal(mapColors(map),c("#4D4D4D", "#6C6C6C", "#838383", 
                                "#969696", "#A7A7A7", "#B5B5B5",  
                                "#C3C3C3", "#CFCFCF", "#DBDBDB", 
                                "#E6E6E6"))
  expect_equal(mapRange(map),c(-7,8))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),25)
  expect_equal(transFunction(map),fnRangeLinear)
  expect_equal(transParameters(map),list(a=1,b=0,min=-5,max=5))
  
  ## (default) MappingStrokeColors factory
  map <- MappingStrokeColors.factory(data=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::stroke"))
  expect_equal(mapColors(map),.default_colors)
  expect_equal(mapRange(map),c(0,1))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),0)
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())

  ## (full args) MappingStrokColors factory
  map <- MappingStrokeColors.factory(data=dummy[,"y"],
                                     targets=circles,
                                     map.colors=c("#4D4D4D", "#6C6C6C", 
                                                  "#838383", "#969696", 
                                                  "#A7A7A7", "#B5B5B5",  
                                                  "#C3C3C3", "#CFCFCF", 
                                                  "#DBDBDB", "#E6E6E6"),
                                     map.range=c(-7,8),
                                     gradient.type="linear",
                                     fill.angle=25,
                                     trans.function=fnRangeLinear,
                                     trans.parameters=list(a=1,b=0,min=-5,max=5))
  expect_equal(values(map),dummy[,"y"])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::stroke")
  expect_equal(mapColors(map),c("#4D4D4D", "#6C6C6C", "#838383", 
                                "#969696", "#A7A7A7", "#B5B5B5",  
                                "#C3C3C3", "#CFCFCF", "#DBDBDB", 
                                "#E6E6E6"))
  expect_equal(mapRange(map),c(-7,8))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),25)
  expect_equal(transFunction(map),fnRangeLinear)
  expect_equal(transParameters(map),list(a=1,b=0,min=-5,max=5))
  
  ## MappingBioArraysColors factory
  map <- MappingBioArraysColors.factory(arrays=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::fill"))
#   expect_equal(mapColors(map),.microarrays_mapping_colors)
  expect_equal(mapRange(map),c(-2,2))
  expect_equal(gradientType(map),"linear")
  expect_equal(fillAngle(map),0)
  expect_equal(transFunction(map),fnIdentity)
  expect_equal(transParameters(map),list())
  
})

