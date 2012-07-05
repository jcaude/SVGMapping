context("SVGNode")

test_that("get/set IDs", {
 
  myClass <- setClass("myClass.SVGNode",
                      contains="SVGNode")
  
  setMethod(f="initialize", signature="myClass.SVGNode",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
            })
  
  node  <- new("myClass.SVGNode")
  expect_equal(id(node), character(0))
  expect_equal(svgTransform(node), character(0))
  expect_is(node,"SVGNode")
  
  id(node) <- "my.ID"
  svgTransform(node) <- "my.Transform"
  
  expect_identical(id(node), "my.ID")
  expect_identical(svgTransform(node), "my.Transform")
  expect_equal(.xml(node), list(id="my.ID", transform="my.Transform"))
})