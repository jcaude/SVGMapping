require(testthat)

context("Mapping (Tooltips)")

test_that("get/set Fields", {
  
  ## standard constructor (empty object)
  ## -- we also check include fields of the 'Mapping' sub-class
  mapping <- new("MappingTooltips")
  
  expect_equal(targets(mapping),NULL)
  expect_equal(values(mapping),NULL)
  expect_equal(transFunction(mapping),NULL)
  expect_equal(transParameters(mapping),list())
  expect_equal(animations(mapping),FALSE)
  expect_is(mapping, "Mapping")
  expect_is(mapping, "MappingTooltips")
  
  ## initialize some specs
  targets(mapping) <- "xpath:://rect[@id='foobar']"
  values(mapping) <- c(1,2,3)
  transFunction(mapping) <- function(x,p) { return(x+p$c) }
  transParameters(mapping) <- list(c=2)
  animations(mapping) <- TRUE
  
  ## check with new specs
  expect_equal(targets(mapping),"xpath:://rect[@id='foobar']")
  expect_equal(values(mapping),c(1,2,3))
  expect_true(is.function(transFunction(mapping)))
  expect_equal(gsub("[ \t]*", "",deparse(transFunction(mapping))),c("function(x,p)","{","return(x+p$c)","}"))
  expect_equal(transParameters(mapping),list(c=2))
  expect_equal(animations(mapping),TRUE)
  
  ## fool accessors
  expect_error(targets(mapping) <- 2)
  expect_error(transFunction(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- "it must failed")
  expect_error(transParameters(mapping) <- list(a=2,"it must failed"))
  expect_error(animations(mapping) <- "it must failed")
})

test_that("Factory Functions", {
  
  ## init.
  circles <- paste("circle.",LETTERS[1:6],sep="")
  dummy <- data.frame(l=c("plane","car","door","dog","door","bus"),
                      row.names=circles, stringsAsFactors=FALSE)
  
  ## (default) MappingValues factory
  map <- MappingTooltips.factory(data=dummy[,"l",drop=FALSE])
  expect_equal(values(map),dummy[,"l",drop=FALSE])
  expect_equal(targets(map),circles)
  expect_equal(transFunction(map),NULL)
  expect_equal(transParameters(map),list())
  
  ## (full args) MappingValues factory
  map <- MappingLinks.factory(data=dummy[,"l"],
                              targets=circles,                        
                              trans.function=fnIdentity,
                              trans.parameters=list()
  )
  expect_equal(values(map),dummy[,"l"])
  expect_equal(targets(map),circles)
  expect_equal(transFunction(map),fnIdentity)
  expect_equal(transParameters(map),list())
})

