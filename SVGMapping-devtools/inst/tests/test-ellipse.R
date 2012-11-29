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
  uzero <- SVGUnit.factory()
  lzero <- SVGLength.factory()
  
  expect_identical(cx(node), uzero)
  expect_identical(cy(node), uzero)
  expect_identical(rx(node), lzero)
  expect_identical(ry(node), lzero)
  expect_is(node,"SVGNode")
  expect_is(node,"Ellipse")
  
  cx(node) <- "10px"
  cy(node) <- "15px"
  rx(node) <- "211pt"
  ry(node) <- "333pt"
  
  expect_identical(cx(node), SVGUnit.factory("10px"))
  expect_identical(cy(node), SVGUnit.factory("15px"))
  expect_identical(rx(node), SVGLength.factory("211pt"))
  expect_identical(ry(node), SVGLength.factory("333pt"))
  
  bbox(node) <- list(cx="9cm",cy="341cm",rx="94cm",ry="8cm")
  
  expect_identical(cx(node), SVGUnit.factory("9cm"))
  expect_identical(cy(node), SVGUnit.factory("341cm"))
  expect_identical(rx(node), SVGLength.factory("94cm"))
  expect_identical(ry(node), SVGLength.factory("8cm"))
  
  expect_equal(bbox(node), list(cx=SVGUnit.factory("9cm"),
                                cy=SVGUnit.factory("341cm"),
                                rx=SVGLength.factory("94cm"),
                                ry=SVGLength.factory("8cm")))
  
  node <- new("myClass.Ellipse",
              bbox=list(cx="9cm",cy="8cm",rx="341cm",ry="94cm"))
  
  expect_identical(cx(node), SVGUnit.factory("9cm"))
  expect_identical(cy(node), SVGUnit.factory("8cm"))
  expect_identical(rx(node), SVGLength.factory("341cm"))
  expect_identical(ry(node), SVGLength.factory("94cm"))
  
  expect_error(new("myClass.Ellipse",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})