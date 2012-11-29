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
  uzero <- SVGCoord.factory()
  lzero <- SVGLength.factory()
  
  expect_equal(x(node), uzero)
  expect_equal(y(node), uzero)
  expect_equal(width(node), lzero)
  expect_equal(height(node), lzero)
  expect_is(node,"SVGNode")
  expect_is(node,"Rectangle")
  
  x(node) <- "10px"
  y(node) <- "15px"
  width(node) <- "211pt"
  height(node) <- "333pt"

  expect_equal(x(node), SVGCoord.factory("10px"))
  expect_equal(y(node), SVGCoord.factory("15px"))
  expect_equal(width(node), SVGLength.factory("211pt"))
  expect_equal(height(node), SVGLength.factory("333pt"))
  
  bbox(node) <- list(x="9cm",width="341cm",height="94cm",y="8cm")
  
  expect_equal(x(node), SVGCoord.factory("9cm"))
  expect_equal(y(node), SVGCoord.factory("8cm"))
  expect_equal(width(node), SVGLength.factory("341cm"))
  expect_equal(height(node), SVGLength.factory("94cm"))
  
  expect_equal(bbox(node), list(x=SVGCoord.factory("9cm"),
                                y=SVGCoord.factory("8cm"),
                                width=SVGLength.factory("341cm"),
                                height=SVGLength.factory("94cm")))
  
  node <- new("myClass.Rectangle",
              bbox=list(x="9cm",y="8cm",width="341cm",height="94cm"))

  expect_equal(x(node), SVGCoord.factory("9cm"))
  expect_equal(y(node), SVGCoord.factory("8cm"))
  expect_equal(width(node), SVGLength.factory("341cm"))
  expect_equal(height(node), SVGLength.factory("94cm"))
  
  expect_error(new("myClass.Rectangle",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})