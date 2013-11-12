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
  
  zero <- SVGCoord.factory()
  
  node  <- new("myClass.Vector")
  expect_identical(x1(node), zero)
  expect_identical(y1(node), zero)
  expect_identical(x2(node), zero)
  expect_identical(y2(node), zero)
  expect_is(node,"SVGNode")
  expect_is(node,"Vector")
  
  x1(node) <- SVGCoord.factory("10px")
  y1(node) <- SVGCoord.factory("15px")
  x2(node) <- SVGCoord.factory("211pt")
  y2(node) <- SVGCoord.factory("333pt")
  
  expect_identical(x1(node), SVGCoord.factory("10px"))
  expect_identical(y1(node), SVGCoord.factory("15px"))
  expect_identical(x2(node), SVGCoord.factory("211pt"))
  expect_identical(y2(node), SVGCoord.factory("333pt"))
  
  coords(node) <- list(x1="9cm",y1="341cm",x2="94cm",y2="8cm")
  
  expect_identical(x1(node), SVGCoord.factory("9cm"))
  expect_identical(y1(node), SVGCoord.factory("341cm"))
  expect_identical(x2(node), SVGCoord.factory("94cm"))
  expect_identical(y2(node), SVGCoord.factory("8cm"))
  
  expect_equal(bbox(node), list(x1=SVGCoord.factory("9cm"),
                                y1=SVGCoord.factory("8cm"),
                                x2=SVGCoord.factory("94cm"),
                                y2=SVGCoord.factory("341cm")))
  
  node <- new("myClass.Vector",
              coords=list(x1="9cm",y1="8cm",x2="341cm",y2="94cm"))
  
  expect_identical(x1(node), SVGCoord.factory("9cm"))
  expect_identical(y1(node), SVGCoord.factory("8cm"))
  expect_identical(x2(node), SVGCoord.factory("341cm"))
  expect_identical(y2(node), SVGCoord.factory("94cm"))
  
  expect_error(new("myClass.Vector",
                   coords=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})