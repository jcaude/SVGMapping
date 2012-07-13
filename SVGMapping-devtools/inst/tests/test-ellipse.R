require(testthat)

context("Ellipse")

test_that("Accessors", {
  
  setClass("myClass.Ellipse",
           contains=c("SVGNode","Ellipse")
  )
  
  setMethod(f="initialize", signature="myClass.Ellipse",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
              .Object <- .callMethod("initialize","Ellipse",.Object,...)
              return(.Object)
            })
  
  node  <- new("myClass.Ellipse")
  expect_identical(cx(node), "0")
  expect_identical(cy(node), "0")
  expect_identical(rx(node), "0")
  expect_identical(ry(node), "0")
  expect_is(node,"SVGNode")
  expect_is(node,"Ellipse")
  
  cx(node) <- "10px"
  cy(node) <- "15px"
  rx(node) <- "211pt"
  ry(node) <- "333pt"
  
  expect_identical(cx(node), "10px")
  expect_identical(cy(node), "15px")
  expect_identical(rx(node), "211pt")
  expect_identical(ry(node), "333pt")
  
  bbox(node) <- list(cx="9cm",cy="341cm",rx="94cm",ry="8cm")
  
  expect_identical(cx(node), "9cm")
  expect_identical(cy(node), "341cm")
  expect_identical(rx(node), "94cm")
  expect_identical(ry(node), "8cm")
  
  expect_equal(bbox(node), list(cx="9cm",cy="341cm",rx="94cm",ry="8cm"))
  
  node <- new("myClass.Ellipse",
              bbox=list(cx="9cm",cy="8cm",rx="341cm",ry="94cm"))
  
  expect_identical(cx(node), "9cm")
  expect_identical(cy(node), "8cm")
  expect_identical(rx(node), "341cm")
  expect_identical(ry(node), "94cm")
  
  expect_error(new("myClass.Ellipse",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})