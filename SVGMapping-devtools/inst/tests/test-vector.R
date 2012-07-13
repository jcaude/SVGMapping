require(testthat)

context("Vector")

test_that("Accessors", {
  
  setClass("myClass.Vector",
           contains=c("SVGNode","Vector")
  )
  
  setMethod(f="initialize", signature="myClass.Vector",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
              .Object <- .callMethod("initialize","Vector",.Object,...)
              return(.Object)
            })
  
  node  <- new("myClass.Vector")
  expect_identical(x1(node), "0")
  expect_identical(y1(node), "0")
  expect_identical(x2(node), "0")
  expect_identical(y2(node), "0")
  expect_is(node,"SVGNode")
  expect_is(node,"Vector")
  
  x1(node) <- "10px"
  y1(node) <- "15px"
  x2(node) <- "211pt"
  y2(node) <- "333pt"
  
  expect_identical(x1(node), "10px")
  expect_identical(y1(node), "15px")
  expect_identical(x2(node), "211pt")
  expect_identical(y2(node), "333pt")
  
  bbox(node) <- list(x1="9cm",y1="341cm",x2="94cm",y2="8cm")
  
  expect_identical(x1(node), "9cm")
  expect_identical(y1(node), "341cm")
  expect_identical(x2(node), "94cm")
  expect_identical(y2(node), "8cm")
  
  expect_equal(bbox(node), list(x1="9cm",y1="341cm",x2="94cm",y2="8cm"))
  
  node <- new("myClass.Vector",
              bbox=list(x1="9cm",y1="8cm",x2="341cm",y2="94cm"))
  
  expect_identical(x1(node), "9cm")
  expect_identical(y1(node), "8cm")
  expect_identical(x2(node), "341cm")
  expect_identical(y2(node), "94cm")
  
  expect_error(new("myClass.Vector",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})