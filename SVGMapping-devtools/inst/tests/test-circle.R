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
  uzero <- SVGUnit.factory()
  lzero <- SVGLength.factory()
  
  expect_identical(cx(node), uzero)
  expect_identical(cy(node), uzero)
  expect_identical(r(node), lzero)
  expect_is(node,"SVGNode")
  expect_is(node,"Circle")
  
  cx(node) <- "10px"
  cy(node) <- "15px"
  r(node) <- "211pt"
  
  expect_identical(cx(node), SVGUnit.factory("10px"))
  expect_identical(cy(node), SVGUnit.factory("15px"))
  expect_identical(r(node), SVGLength.factory("211pt"))
  
  bbox(node) <- list(cx="9cm",cy="341cm",r="94cm")
  
  expect_identical(cx(node), SVGUnit.factory("9cm"))
  expect_identical(cy(node), SVGUnit.factory("341cm"))
  expect_identical(r(node), SVGLength.factory("94cm"))
  
  expect_equal(bbox(node), list(cx=SVGUnit.factory("9cm"),
                                cy=SVGUnit.factory("341cm"),
                                r=SVGLength.factory("94cm")))
  
  node <- new("myClass.Circle",
              bbox=list(cx="9cm",cy="8cm",r="341cm"))
  
  expect_identical(cx(node), SVGUnit.factory("9cm"))
  expect_identical(cy(node), SVGUnit.factory("8cm"))
  expect_identical(r(node), SVGLength.factory("341cm"))
  
  expect_error(new("myClass.Circle",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})