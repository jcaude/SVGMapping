require(testthat)

context("SVG Rectangle")

test_that("Accessors", {
  
  rectangle <- SVGRect.factory()
  uzero <- SVGCoord.factory()
  lzero <- SVGLength.factory()

  expect_equal(id(rectangle), character(0))
  expect_equal(svgTransform(rectangle),character(0))
  expect_equal(cssClass(rectangle),character(0))
  expect_equal(cssStyle(rectangle),character(0))
  expect_identical(x(rectangle),uzero)
  expect_identical(y(rectangle),uzero)
  expect_identical(width(rectangle),lzero)
  expect_identical(height(rectangle),lzero)
  expect_equal(roundx(rectangle),lzero)
  expect_equal(roundy(rectangle),lzero)
  expect_is(rectangle,"SVGShape")
  expect_is(rectangle,"Rectangle")

  id(rectangle) <- "rectangle1"
  bbox(rectangle) <- list(x="10px",y="20px",width="100px",height="70px")
  roundx(rectangle) <- 5
  roundy(rectangle) <- 7
  cssClass(rectangle) <- "rectangle.style"
  cssStyle(rectangle) <- "stroke:blue; stroke-width:2pt"
  svgTransform(rectangle) <- "translate(5,6)"
  
  expect_identical(id(rectangle), "rectangle1")
  expect_identical(svgTransform(rectangle),"translate(5,6)")
  expect_identical(cssClass(rectangle),"rectangle.style")
  expect_identical(cssStyle(rectangle),"stroke:blue; stroke-width:2pt")
  expect_identical(x(rectangle),SVGCoord.factory("10px"))
  expect_identical(y(rectangle),SVGCoord.factory("20px"))
  expect_identical(width(rectangle),SVGLength.factory("100px"))
  expect_identical(height(rectangle),SVGLength.factory("70px"))
  expect_identical(roundx(rectangle),SVGLength.factory("5"))
  expect_identical(roundy(rectangle),SVGLength.factory("7"))
  expect_is(rectangle,"SVGShape")
  expect_is(rectangle,"Rectangle")
  
})
          
test_that("Rendering", {

  rectangle <- SVGRect.factory(bbox=list(x="10px",y="20px",width="100px",height="70px"),
                               roundx="2px", roundy="3px",
                               class="rectangle.style",
                               style="stroke:blue; stroke-width:2pt",
                               transform="translate(5,6)")
  id(rectangle) <- "rectangle1"
  
  expect_output(.xml(rectangle),"<rect id=\"rectangle1\" transform=\"translate\\(5,6\\)\" class=\"rectangle.style\" style=\"stroke:blue; stroke-width:2pt\" x=\"10px\" y=\"20px\" width=\"100px\" height=\"70px\" rx=\"2px\" ry=\"3px\"/>.*")
  
})