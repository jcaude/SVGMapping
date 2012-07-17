require(testthat)

context("Linear Gradient")

test_that("get/set Fields", {
  
  ## Standard constructor (empty)
  gradient  <- new("LinearGradient")
  
  expect_identical(svgUnits(gradient), "objectBoundingBox")
  expect_identical(spreadMethod(gradient), "pad")
  expect_equal(xlinkHref(gradient), character(0))
  expect_equal(stops(gradient), list())
  expect_identical(x1(gradient), "0")
  expect_identical(y1(gradient), "0")
  expect_identical(x2(gradient), "0")
  expect_identical(y2(gradient), "0")
  expect_is(gradient, "Gradient")
  expect_is(gradient, "Vector")
  
  expect_error(svgUnits(gradient) <- "this must not work")
  expect_error(spreadMethod(gradient) <- "this must not work")
  
  ## initialize some specs
  svgUnits(gradient) <- "userSpaceOnUse"
  spreadMethod(gradient) <- "repeat"
  xlinkHref(gradient) <- "http://mywebsite.org/we-need-you"
  x1(gradient) <- "1px"
  y1(gradient) <- "5px"
  x2(gradient) <- "21pt"
  y2(gradient) <- "33pt"
  stops(gradient) <- list(GradientStop.factory(0,"yellow"),
                          GradientStop.factory(0.3,"brown",0.9),
                          GradientStop.factory(0.6,"black"),
                          GradientStop.factory(1,"grey",0.9))
  id(gradient) <- "LinGradient_01"
  
  
  expect_identical(svgUnits(gradient), "userSpaceOnUse")
  expect_identical(spreadMethod(gradient), "repeat")
  expect_identical(xlinkHref(gradient), "http://mywebsite.org/we-need-you")
  expect_identical(x1(gradient), "1px")
  expect_identical(y1(gradient), "5px")
  expect_identical(x2(gradient), "21pt")
  expect_identical(y2(gradient), "33pt")
  expect_equal(length(stops(gradient)),4)
  expect_equal(id(gradient), "LinGradient_01")
  expect_identical(URL(gradient), "url(#LinGradient_01)")
  
  expect_output(print(.xml(gradient)), "<linearGradient id=\"LinGradient_01\" gradientUnits=\"userSpaceOnUse\" spreadMethod=\"repeat\" xlink:href=\"http://mywebsite.org/we-need-you\" x1=\"1px\" y1=\"5px\" x2=\"21pt\" y2=\"33pt\">[ \t\n]*<stop offset=\"0\" stop-color=\"yellow\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"0.3\" stop-color=\"brown\" stop-opacity=\"0.9\"/>[ \t\n]*<stop offset=\"0.6\" stop-color=\"black\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"1\" stop-color=\"grey\" stop-opacity=\"0.9\"/>[ \t\n]*</linearGradient>")
  
  ## Factory building
  blue <- GradientStop.factory(color="blue")
  red.50 <- GradientStop.factory(offset="1",color="red",opacity=0.5) 
  gradient <- LinearGradient.factory(stops=c(blue,red.50), bbox=list(x1="0",y1="0",x2="1",y2="0"))
  
  expect_output(print(.xml(gradient)),"<linearGradient>[ \t\n]*<stop offset=\"0\" stop-color=\"blue\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"1\" stop-color=\"red\" stop-opacity=\"0.5\"/>[ \t\n]*</linearGradient>")
})
