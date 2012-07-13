require(testthat)

context("Layout")

test_that("get/set Locations attributes", {
  
  myClass <- setClass("myClass.Layout",
                      contains="Layout"
  )
  
  setMethod(f="initialize", signature="myClass.Layout",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
            })
  
  layout  <- new("myClass.Layout")
  
  expect_equal(opacity(layout), 0.0)
  expect_equal(bbox(layout), list(x="0",y="0",width="0",height="0"))
  expect_is(layout, "Layout")
  expect_is(layout, "SVGNode")
  expect_is(layout, "Rectangle")
  
  opacity(layout) <- 0.8
  x(layout) <- 0.2
  y(layout) <- 0.21
  width(layout) <- 200.5
  height(layout) <- 99.4
  
  expect_equal(opacity(layout), 0.8)
  expect_equal(x(layout), "0.2")
  expect_equal(y(layout), "0.21")
  expect_equal(width(layout), "200.5")
  expect_equal(height(layout), "99.4")
  expect_equal(.xml(layout),list(x="0.2",y="0.21",width="200.5",height="99.4",opacity=0.8))
  
  id(layout) <- "ID10"
  
  expect_equal(.xml(layout),list(id="ID10",
                                 x="0.2",y="0.21",width="200.5",height="99.4",
                                 opacity=0.8))
})
