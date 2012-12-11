require(testthat)

context("SVG Ellipse")

test_that("Accessors", {
  
  ellipse <- SVGEllipse.factory()
  uzero <- SVGCoord.factory()
  lzero <- SVGLength.factory()
  
  expect_equal(id(ellipse), character(0))
  expect_equal(svgTransform(ellipse),character(0))
  expect_equal(cssClass(ellipse),character(0))
  expect_equal(cssStyle(ellipse),character(0))
  expect_identical(cx(ellipse),uzero)
  expect_identical(cy(ellipse),uzero)
  expect_identical(rx(ellipse),lzero)
  expect_identical(ry(ellipse),lzero)
  expect_is(ellipse,"SVGShape")
  expect_is(ellipse,"Ellipse")
  
  id(ellipse) <- "ellipse1"
  bbox(ellipse) <- list(cx="10px",cy="20px",rx="100px",ry="70px")
  cssClass(ellipse) <- "ellipse.style"
  cssStyle(ellipse) <- "stroke:blue; stroke-width:2pt"
  svgTransform(ellipse) <- "translate(5,6)"
  
  expect_identical(id(ellipse), "ellipse1")
  expect_identical(svgTransform(ellipse),"translate(5,6)")
  expect_identical(cssClass(ellipse),"ellipse.style")
  expect_identical(cssStyle(ellipse),"stroke:blue; stroke-width:2pt")
  expect_identical(cx(ellipse),SVGCoord.factory("10px"))
  expect_identical(cy(ellipse),SVGCoord.factory("20px"))
  expect_identical(rx(ellipse),SVGLength.factory("100px"))
  expect_identical(ry(ellipse),SVGLength.factory("70px"))
  expect_is(ellipse,"SVGShape")
  expect_is(ellipse,"Ellipse")
  
})

test_that("Factory", {
  
  ellipse <- SVGEllipse.factory(bbox=list(cx="10px",cy="20px",rx="100px",ry="70px"),
                                class="ellipse.style",
                                style="stroke:blue; stroke-width:2pt",
                                transform="translate(5,6)")
  id(ellipse) <- "ellipse1"
  
  expect_output(print(.xml(ellipse)),"<ellipse id=\"ellipse1\" class=\"ellipse.style\" style=\"stroke:blue; stroke-width:2pt\" transform=\"translate\\(5,6\\)\" cx=\"10px\" cy=\"20px\" rx=\"100px\" ry=\"70px\"/>.*")
  
})