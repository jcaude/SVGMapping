context("Gradient")

myClass <- setClass("myClass.Gradient",
                    contains="Gradient"
)

setMethod(f="initialize", signature="myClass.Gradient",
          definition=function(.Object,...)
          {
            .Object <- callNextMethod(.Object,...)
          })

test_that("get/set Fields", {
  
  gradient  <- new("myClass.Gradient")
  
  expect_identical(svgUnits(gradient), "objectBoundingBox")
  expect_identical(spreadMethod(gradient), "pad")
  expect_equal(xlinkHref(gradient), character(0))
  expect_equal(stops(gradient), list())
  expect_is(gradient, "Gradient")
  expect_is(gradient, "SVGNode")
  
  expect_error(svgUnits(gradient) <- "this must not work")
  expect_error(spreadMethod(gradient) <- "this must not work")
  
  svgUnits(gradient) <- "userSpaceOnUse"
  spreadMethod(gradient) <- "repeat"
  xlinkHref(gradient) <- "http://mywebsite.org/we-need-you"
  
  expect_identical(svgUnits(gradient), "userSpaceOnUse")
  expect_identical(spreadMethod(gradient), "repeat")
  expect_identical(xlinkHref(gradient), "http://mywebsite.org/we-need-you")
  
})

## Test Gradient stops..
test_that("Gradient(stops)", {
  
  gradient  <- new("myClass.Gradient")
  
  expect_equal(length(stops(gradient)), 0)
  expect_error(stops(gradient) <- list(1,2,3,4,5))
  
  stops(gradient) <- list(GradientStop.factory(0,"blue"),
                          GradientStop.factory(1,"red",0.5))
  id(gradient) <- "gradient01"
  
  expect_equal(length(stops(gradient)),2)
  expect_equal(.xml(gradient), list(id="gradient01"))
  expect_identical(URL(gradient), "url(#gradient01)")
})
