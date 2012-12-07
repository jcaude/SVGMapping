require(testthat)

context("CSS")

test_that("Accessors", {
  
  myClass <- setClass("myClass.CSS",
                      contains=c("SVGNode","CSS")
  )
  
  setMethod(f="initialize", signature="myClass.CSS",
            definition=function(.Object,...)
            {
              .Object <- callNextMethod(.Object,...)
              .Object <- .callMethod("initialize","CSS",.Object,...)
              return(.Object)
            })
  
  node  <- new("myClass.CSS")
  expect_equal(cssClass(node), character(0))
  expect_equal(cssStyle(node), character(0))
  expect_is(node,"CSS")
  expect_is(node,"SVGNode")
  
  cssClass(node) <- "my.Class"
  cssStyle(node) <- "foo:bar"
  
  expect_identical(cssClass(node), "my.Class")
  expect_identical(cssStyle(node), "foo:bar")
  expect_equal(node["foo"],"bar")
  
  node["john"] <- "doe"
  expect_equal(cssStyle(node),"foo:bar; john:doe")
  expect_equal(node["john"],"doe")
})