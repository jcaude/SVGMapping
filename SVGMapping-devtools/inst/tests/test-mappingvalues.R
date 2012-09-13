require(testthat)

context("Mapping (Values)")

test_that("get/set Fields", {
  
  ## standard constructor (empty object)
  ## -- we also check include fields of the 'Mapping' sub-class
  mapping <- new("MappingValues")
  
  expect_equal(targets(mapping),NULL)
  expect_equal(values(mapping),NULL)
  expect_equal(transFunction(mapping),NULL)
  expect_equal(transParameters(mapping),list())
  expect_equal(animations(mapping),FALSE)
  expect_equal(targetAttribute(mapping),character(0))
  expect_equal(targetUnit(mapping),vector("character"))
  expect_is(mapping, "Mapping")
  expect_is(mapping, "MappingValues")
  
  ## initialize some specs
  targets(mapping) <- "xpath:://rect[@id='foobar']"
  values(mapping) <- c(1,2,3)
  transFunction(mapping) <- function(x,p) { return(x+p$c) }
  transParameters(mapping) <- list(c=2)
  animations(mapping) <- TRUE
  targetAttribute(mapping) <- "style::width"
  targetUnit(mapping) <- "cm"
  
  ## check with new specs
  expect_equal(targets(mapping),"xpath:://rect[@id='foobar']")
  expect_equal(values(mapping),c(1,2,3))
  expect_true(is.function(transFunction(mapping)))
  expect_equal(gsub("[ \t]*", "",deparse(transFunction(mapping))),c("function(x,p)","{","return(x+p$c)","}"))
  expect_equal(transParameters(mapping),list(c=2))
  expect_equal(animations(mapping),TRUE)
  expect_equal(targetAttribute(mapping),"style::width")
  expect_equal(targetUnit(mapping),c("cm"))
  
  ## fool accessors
  expect_error(targets(mapping) <- 2)
  expect_error(transFunction(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- list(a=2,"it must failed"))
  expect_error(animations(mapping) <- "it must failed")
  expect_error(targetAttribute(mapping) <- 15)
})

test_that("Factory Functions", {
  
  ## init.
  circles <- paste("circle.",LETTERS[1:6],sep="")
  z <- runif(6,min=-5,max=5)
  dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
                      y=rep(3,times=6),
                      z=z,
                      row.names=circles)
  
  ## (default) MappingValues factory
  map <- MappingValues.factory(data=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),character(0))
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())

  ## (full args) MappingValues factory
  map <- MappingValues.factory(data=dummy[,"y"],
                               targets=circles,
                               target.attribute=c("style::stroke-width"),
                               target.unit="cm",
                               trans.function=fnRandom,
                               trans.parameters=list(min=-3.9,max=6.6))
  expect_equal(values(map),dummy[,"y"])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),c("style::stroke-width"))
  expect_equal(targetUnit(map),c("cm"))
  expect_equal(transFunction(map),fnRandom)
  expect_equal(transParameters(map),list(min=-3.9,max=6.6))
  
  ## (default) MappingOpacity factory
  map <- MappingOpacity.factory(data=dummy[,"y",drop=FALSE])
  expect_equal(values(map),dummy[,"y",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnRangeLinear)
  expect_equal(transParameters(map),list(a=1,b=0,min=0,max=1))
  
  ## (full args) MappingOpacity factory
  map <- MappingOpacity.factory(data=dummy[,"z"],
                                targets=as.character(1:6),
                                trans.function=fnLinear,
                                trans.parameters=list(a=1/10,b=5))
  expect_equal(values(map),z)
  expect_equal(targets(map),as.character(1:6))
  expect_equal(targetAttribute(map),"opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnLinear)
  expect_equal(transParameters(map),list(a=1/10,b=5))

  ## (default) MappingFillOpacity factory
  map <- MappingFillOpacity.factory(data=dummy[,"y",drop=FALSE])
  expect_equal(values(map),dummy[,"y",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::fill-opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnRangeLinear)
  expect_equal(transParameters(map),list(a=1,b=0,min=0,max=1))
  
  ## (full args) MappingFillOpacity factory
  map <- MappingFillOpacity.factory(data=dummy[,"z"],
                                    targets=as.character(1:6),
                                    trans.function=fnLinear,
                                    trans.parameters=list(a=1/10,b=5))
  expect_equal(values(map),z)
  expect_equal(targets(map),as.character(1:6))
  expect_equal(targetAttribute(map),"style::fill-opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnLinear)
  expect_equal(transParameters(map),list(a=1/10,b=5))
  
  ## (default) MappingStrokeOpacity factory
  map <- MappingStrokeOpacity.factory(data=dummy[,"y",drop=FALSE])
  expect_equal(values(map),dummy[,"y",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::stroke-opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnRangeLinear)
  expect_equal(transParameters(map),list(a=1,b=0,min=0,max=1))
  
  ## (full args) MappingStrokeOpacity factory
  map <- MappingStrokeOpacity.factory(data=dummy[,"z"],
                                      targets=as.character(1:6),
                                      trans.function=fnLinear,
                                      trans.parameters=list(a=1/10,b=5))
  expect_equal(values(map),z)
  expect_equal(targets(map),as.character(1:6))
  expect_equal(targetAttribute(map),"style::stroke-opacity")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),fnLinear)
  expect_equal(transParameters(map),list(a=1/10,b=5))

  ## (default) MappingStrokeWidth factory
  map <- MappingStrokeWidth.factory(data=dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::stroke-width")
  expect_equal(targetUnit(map),vector("character"))
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())
  
  ## (full args) MappingStrokeWidth factory
  map <- MappingStrokeWidth.factory(data=dummy[,"y"],
                                    targets=circles,
                                    target.unit="cm",
                                    trans.function=fnLinear,
                                    trans.parameters=list(a=1.35,b=0))
  expect_equal(values(map),dummy[,"y"])
  expect_equal(targets(map),circles)
  expect_equal(targetAttribute(map),"style::stroke-width")
  expect_equal(targetUnit(map),"cm")
  expect_equal(transFunction(map),fnLinear)
  expect_equal(transParameters(map),list(a=1.35,b=0))
  
})

