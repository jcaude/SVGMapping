context("SVGShape")

test_that("get/set CORE attributes", {
  
  myClass <- setClass("myClass.SVGShape",
                      contains="SVGShape"
  )
  
  setMethod(f="initialize", signature="myClass.SVGShape",
          definition=function(.Object,...)
          {
            .Object <- callNextMethod(.Object,...)
          })
  
  shape  <- new("myClass.SVGShape")

  expect_equal(cssClass(shape), character(0))
  expect_equal(cssStyle(shape), character(0))
  expect_is(shape, "SVGShape")
  expect_is(shape,"SVGNode")
  expect_is(shape,"CSS")
  
  cssClass(shape) <- "my.Class"
  cssStyle(shape) <- "my.Style"
  
  expect_identical(cssClass(shape), "my.Class")
  expect_identical(cssStyle(shape), "my.Style")
  expect_equal(.xml(shape), list(class="my.Class", style="my.Style"))

})