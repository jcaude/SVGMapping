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
  expect_identical(x(node), "0")
  expect_identical(y(node), "0")
  expect_identical(width(node), "0")
  expect_identical(height(node), "0")
  expect_is(node,"SVGNode")
  expect_is(node,"Rectangle")
  
  x(node) <- "10px"
  y(node) <- "15px"
  width(node) <- "211pt"
  height(node) <- "333pt"

  expect_identical(x(node), "10px")
  expect_identical(y(node), "15px")
  expect_identical(width(node), "211pt")
  expect_identical(height(node), "333pt")
  
  bbox(node) <- list(x="9cm",width="341cm",height="94cm",y="8cm")
  
  expect_identical(x(node), "9cm")
  expect_identical(y(node), "8cm")
  expect_identical(width(node), "341cm")
  expect_identical(height(node), "94cm")
  
  expect_equal(bbox(node), list(x="9cm",y="8cm",width="341cm",height="94cm"))
  
  node <- new("myClass.Rectangle",
              bbox=list(x="9cm",y="8cm",width="341cm",height="94cm"))

  expect_identical(x(node), "9cm")
  expect_identical(y(node), "8cm")
  expect_identical(width(node), "341cm")
  expect_identical(height(node), "94cm")
  
  expect_error(new("myClass.Rectangle",
                   bbox=list(a="9cm",b="8cm",c="341cm",d="94cm")))
})