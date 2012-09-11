require(testthat)

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
  expect_equal(mapColors(mapping),vector())
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
  
  ## MappingColors factory
})

