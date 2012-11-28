require(testthat)

context("Mapping (Masks)")

test_that("get/set Fields", {
  
  ## standard constructor (empty object)
  ## -- we also check include fields of the 'Mapping' sub-class
  mapping <- new("MappingMasks")
  
  expect_equal(targets(mapping),NULL)
  expect_equal(values(mapping),NULL)
  expect_equal(transFunction(mapping),NULL)
  expect_equal(transParameters(mapping),list())
  expect_equal(animations(mapping),FALSE)
  expect_equal(fillAngle(mapping),-pi/2)
  expect_is(mapping, "Mapping")
  expect_is(mapping, "MappingMasks")
  
  ## initialize some specs
  targets(mapping) <- "xpath:://rect[@id='foobar']"
  values(mapping) <- c(1,2,3)
  transFunction(mapping) <- function(x,p) { return(x+p$c) }
  transParameters(mapping) <- list(c=2)
  animations(mapping) <- TRUE
  fillAngle(mapping) <- pi*0.75
  
  ## check with new specs
  expect_equal(targets(mapping),"xpath:://rect[@id='foobar']")
  expect_equal(values(mapping),c(1,2,3))
  expect_true(is.function(transFunction(mapping)))
  expect_equal(gsub("[ \t]*", "",deparse(transFunction(mapping))),c("function(x,p)","{","return(x+p$c)","}"))
  expect_equal(transParameters(mapping),list(c=2))
  expect_equal(animations(mapping),TRUE)
  expect_equal(fillAngle(mapping),0.75*pi)
  
  ## fool accessors
  expect_error(targets(mapping) <- 2)
  expect_error(transFunction(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- list(a=2,"it must failed"))
  expect_error(animations(mapping) <- "it must failed")
  expect_error(fillAngle(mapping) <- "definitely wrong")
  expect_error(fillAngle(mapping) <- 1.5*pi)
})

test_that("Factory Functions", {
  
  ## init.
  circles <- paste("circle.",LETTERS[1:6],sep="")
  dummy <- data.frame(x=c(0,0.2,0.4,0.6,0.8,1.0),
                      row.names=circles)
  
  ## (default) MappingValues factory
  map <- MappingMasks.factory(dummy[,"x",drop=FALSE])
  expect_equal(values(map),dummy[,"x",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())
  expect_equal(fillAngle(map),-pi/2)
  
  ## (full args) MappingValues factory
  map <- MappingMasks.factory(data=dummy[,"x"],
                              fill.angle=0.0,
                              targets=circles,                        
                              trans.function=fnIdentity,
                              trans.parameters=list()
  )
  expect_equal(values(map),dummy[,"x"])
  expect_equal(targets(map),circles)
  expect_equal(transFunction(map),fnIdentity)
  expect_equal(transParameters(map),list())
  expect_equal(fillAngle(map),0)
})