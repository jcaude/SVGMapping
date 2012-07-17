require(testthat)

context("Circle")

test_that("Accessors", {
  
  setClass("myClass.Circle",
           contains=c("SVGNode","Circle")
  )
  
  setMethod(f="initialize", signature="myClass.Circle",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
              .Object <- .callMethod("initialize","Circle",.Object,...)
              return(.Object)
            })
  
  node  <- new("myClass.Circle")
  expect_identical(cx(node), "0")
  expect_identical(cy(node), "0")
  expect_identical(r(node), "0")
  expect_is(node,"SVGNode")
  expect_is(node,"Circle")
  
  cx(node) <- "10px"
  cy(node) <- "15px"
  r(node) <- "211pt"
  
  expect_identical(cx(node), "10px")
  expect_identical(cy(node), "15px")
  expect_identical(r(node), "211pt")
  
  bbox(node) <- list(cx="9cm",cy="341cm",r="94cm")
  
  expect_identical(cx(node), "9cm")
  expect_identical(cy(node), "341cm")
  expect_identical(r(node), "94cm")
  
  expect_equal(bbox(node), list(cx="9cm",cy="341cm",r="94cm"))
  
  node <- new("myClass.Circle",
              bbox=list(cx="9cm",cy="8cm",r="341cm"))
  
  expect_identical(cx(node), "9cm")
  expect_identical(cy(node), "8cm")
  expect_identical(r(node), "341cm")
  
  expect_error(new("myClass.Circle",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})