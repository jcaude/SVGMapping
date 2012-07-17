require(testthat)

context("Radial Gradient")

test_that("get/set Fields", {
  
  ## Standard constructor (empty)
  gradient  <- new("RadialGradient")
  
  expect_identical(svgUnits(gradient), "objectBoundingBox")
  expect_identical(spreadMethod(gradient), "pad")
  expect_equal(xlinkHref(gradient), character(0))
  expect_equal(stops(gradient), list())
  expect_identical(cx(gradient), "0")
  expect_identical(cy(gradient), "0")
  expect_identical(r(gradient), "0")
  expect_identical(fx(gradient), character(0))
  expect_identical(fy(gradient), character(0))  
  expect_is(gradient, "Gradient")
  expect_is(gradient, "Circle")
  
  expect_error(svgUnits(gradient) <- "this must not work")
  expect_error(spreadMethod(gradient) <- "this must not work")
  
  ## Fill some specs...
  svgUnits(gradient) <- "userSpaceOnUse"
  spreadMethod(gradient) <- "repeat"
  xlinkHref(gradient) <- "http://mywebsite.org/we-need-you"
  bbox(gradient) <- list(cx="15pt",cy="12pt",r="50px")
  fx(gradient) <- "4"
  fy(gradient) <- "3"
  stops(gradient) <- list(GradientStop.factory(0,"white"),
                          GradientStop.factory(0.2,"darkbrown",0.48),
                          GradientStop.factory(0.8,"purple"),
                          GradientStop.factory(1,"lightgrey",0.67))
  id(gradient) <- "RadialGradientX01"
  
  expect_identical(svgUnits(gradient), "userSpaceOnUse")
  expect_identical(spreadMethod(gradient), "repeat")
  expect_identical(xlinkHref(gradient), "http://mywebsite.org/we-need-you")
  expect_identical(cx(gradient), "15pt")
  expect_identical(cy(gradient), "12pt")
  expect_identical(r(gradient), "50px")
  expect_identical(fx(gradient), "4")
  expect_identical(fy(gradient), "3")
  expect_equal(length(stops(gradient)),4)
  expect_equal(id(gradient), "RadialGradientX01")
  expect_identical(URL(gradient), "url(#RadialGradientX01)")
  
  expect_output(print(.xml(gradient)),"<radialGradient id=\"RadialGradientX01\" gradientUnits=\"userSpaceOnUse\" spreadMethod=\"repeat\" xlink:href=\"http://mywebsite.org/we-need-you\" cx=\"15pt\" cy=\"12pt\" r=\"50px\" fx=\"4\" fy=\"3\">[ \t\n]*<stop offset=\"0\" stop-color=\"white\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"0.2\" stop-color=\"darkbrown\" stop-opacity=\"0.48\"/>[ \t\n]*<stop offset=\"0.8\" stop-color=\"purple\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"1\" stop-color=\"lightgrey\" stop-opacity=\"0.67\"/>[ \t\n]*</radialGradient>")
  
  ## Factory building
  blue <- GradientStop.factory(color="blue")
  red.50 <- GradientStop.factory(offset="1",color="red",opacity=0.5) 
  gradient <- RadialGradient.factory(stops=c(blue,red.50), bbox=list(cx="0",cy="0",r="40%"))
  
  expect_output(print(.xml(gradient)), "<radialGradient r=\"40%\">[ \t\n]*<stop offset=\"0\" stop-color=\"blue\" stop-opacity=\"1\"/>[ \t\n]*<stop offset=\"1\" stop-color=\"red\" stop-opacity=\"0.5\"/>[ \t\n]*</radialGradient>")
  
})