require(testthat)

context("Rectangle")

test_that("Accessors", {
  
  setClass("myClass.Rectangle",
           contains=c("SVGNode","Rectangle")
  )
  
  setMethod(f="initialize", signature="myClass.Rectangle",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
              .Object <- .callMethod("initialize","Rectangle",.Object,...)
              return(.Object)
            })
  
  node  <- new("myClass.Rectangle")
  zero <- SVGUnit.factory()
  
  expect_equal(x(node), zero)
  expect_equal(y(node), zero)
  expect_equal(width(node), zero)
  expect_equal(height(node), zero)
  expect_is(node,"SVGNode")
  expect_is(node,"Rectangle")
  
  x(node) <- "10px"
  y(node) <- "15px"
  width(node) <- "211pt"
  height(node) <- "333pt"

  expect_equal(x(node), SVGUnit.factory("10px"))
  expect_equal(y(node), SVGUnit.factory("15px"))
  expect_equal(width(node), SVGUnit.factory("211pt"))
  expect_equal(height(node), SVGUnit.factory("333pt"))
  
  bbox(node) <- list(x="9cm",width="341cm",height="94cm",y="8cm")
  
  expect_equal(x(node), SVGUnit.factory("9cm"))
  expect_equal(y(node), SVGUnit.factory("8cm"))
  expect_equal(width(node), SVGUnit.factory("341cm"))
  expect_equal(height(node), SVGUnit.factory("94cm"))
  
  expect_equal(bbox(node), list(x=SVGUnit.factory("9cm"),
                                y=SVGUnit.factory("8cm"),
                                width=SVGUnit.factory("341cm"),
                                height=SVGUnit.factory("94cm")))
  
  node <- new("myClass.Rectangle",
              bbox=list(x="9cm",y="8cm",width="341cm",height="94cm"))

  expect_equal(x(node), SVGUnit.factory("9cm"))
  expect_equal(y(node), SVGUnit.factory("8cm"))
  expect_equal(width(node), SVGUnit.factory("341cm"))
  expect_equal(height(node), SVGUnit.factory("94cm"))
  
  expect_error(new("myClass.Rectangle",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})