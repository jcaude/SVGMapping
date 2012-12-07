require(testthat)

context("SVG Circle")

test_that("Accessors", {
  
  circle <- SVGCircle.factory()
  uzero <- SVGCoord.factory()
  lzero <- SVGLength.factory()
  
  expect_equal(id(circle), character(0))
  expect_equal(svgTransform(circle),character(0))
  expect_equal(cssClass(circle),character(0))
  expect_equal(cssStyle(circle),character(0))
  expect_identical(cx(circle),uzero)
  expect_identical(cy(circle),uzero)
  expect_identical(r(circle),lzero)
  expect_is(circle,"SVGShape")
  expect_is(circle,"Circle")
  
  id(circle) <- "circle1"
  bbox(circle) <- list(cx="10px",cy="20px",r="70px")
  cssClass(circle) <- "circle.style"
  cssStyle(circle) <- "stroke:blue; stroke-width:2pt"
  svgTransform(circle) <- "translate(5,6)"
  
  expect_identical(id(circle), "circle1")
  expect_identical(svgTransform(circle),"translate(5,6)")
  expect_identical(cssClass(circle),"circle.style")
  expect_identical(cssStyle(circle),"stroke:blue; stroke-width:2pt")
  expect_identical(cx(circle),SVGCoord.factory("10px"))
  expect_identical(cy(circle),SVGCoord.factory("20px"))
  expect_identical(r(circle),SVGLength.factory("70px"))
  expect_is(circle,"SVGShape")
  expect_is(circle,"Circle")
  
})

test_that("Factory", {
  
  circle <- SVGCircle.factory(bbox=list(cx="10px",cy="20px",r="100px"),
                              class="circle.style",
                              style="stroke:blue; stroke-width:2pt",
                              transform="translate(5,6)")
  id(circle) <- "circle1"
  
  expect_output(print(.xml(circle)),"<circle id=\"circle1\" transform=\"translate\\(5,6\\)\" class=\"circle.style\" style=\"stroke:blue; stroke-width:2pt\" cx=\"10px\" cy=\"20px\" r=\"100px\"/>.*")
  
})