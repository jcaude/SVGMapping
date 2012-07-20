require(testthat)

context("Mapping")

myClass <- setClass("myClass.Mapping",
                    contains="Mapping"
)

setMethod(f="initialize", signature="myClass.Mapping",
          definition=function(.Object,...)
          {
            .Object <- callNextMethod(.Object,...)
          })

test_that("get/set Fields", {
  
  ## standard constructor (empty object)
  mapping  <- new("myClass.Mapping")
  
  expect_equal(targets(mapping),NULL)
  expect_equal(values(mapping),NULL)
  expect_equal(transFunction(mapping),NULL)
  expect_equal(transParameters(mapping),list())
  expect_equal(animations(mapping),FALSE)
  expect_is(mapping, "Mapping")
  
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

test_that("Transformation Functions", {
  
  ## build the test mapping instance
  mapping <- new("myClass.Mapping")
  targets(mapping) <- "xpath:://rect[@id='foobar']"
  values(mapping) <- c(-5:5)
  
  ## Default Transformation
  exec(mapping,NULL)
  expect_equal(mapping@.values,values(mapping))
  
  ## Random Transformation
  set.seed(1234)
  expected_values <- runif(length(-5:5),0,1)
  set.seed(1234)
  transFunction(mapping) <- MAPPING.Random
  transParameters(mapping) <- list(min=0,max=1)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## Identity Transformation
  transFunction(mapping) <- MAPPING.Identity
  transParameters(mapping) <- list()
  exec(mapping,NULL)
  expect_equal(mapping@.values,-5:5)
  
  ## Linear Transformation
  expected_values <- 4.345 * -5:5 + 3.12
  transFunction(mapping) <- MAPPING.Linear
  transParameters(mapping) <- list(a=4.345,b=3.12)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## Rang-Linear Transformation
  expected_values <- pmax(-10,pmin(4.345 * -5:5 + 3.12,10))
  transFunction(mapping) <- MAPPING.RangeLinear
  transParameters(mapping) <- list(a=4.345,b=3.12,min=-10,max=10)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## Logistic Transformation
  expected_values <- 0.9/(1+5*exp(- 1.2 * -5:5))
  transFunction(mapping) <- MAPPING.Logistic
  transParameters(mapping) <- list(K=0.9,a=5,r=1.2)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## Sigmoid Transformation
  expected_values <- 1/(1+exp(- 0.54 * -5:5))
  transFunction(mapping) <- MAPPING.Sigmoid
  transParameters(mapping) <- list(r=0.54)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## User-defined Transformation
  my.func <- function(x,p) {return(besselI(abs(x),p$nu))}
  expected_values <- besselI(abs(-5:5),2.34)
  transFunction(mapping) <- my.func
  transParameters(mapping) <- list(nu=2.34)
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
  ## Log2FC Transformation
  ratiosDN <- runif(10,min=-5,max=0)
  ratiosUP <- runif(10,min=0,max=5)
  ratios <- c(ratiosDN,ratiosUP)
  values(mapping) <- ratios
  expected_values <- c(-1/exp(ratiosDN*log(2)),exp(ratiosUP*log(2)))
  transFunction(mapping) <- MAPPING.Log2FC
  transParameters(mapping) <- list()
  exec(mapping,NULL)
  expect_equal(mapping@.values,expected_values)
  
})