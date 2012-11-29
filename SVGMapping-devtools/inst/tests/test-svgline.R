require(testthat)

context("SVG Line")

test_that("Accessors", {
  
  line <- SVGLine.factory()
  zero <- SVGCoord.factory()
  
  expect_equal(id(line), character(0))
  expect_equal(svgTransform(line),character(0))
  expect_equal(cssClass(line),character(0))
  expect_equal(cssStyle(line),character(0))
  expect_equal(x1(line),zero)
  expect_equal(y1(line),zero)
  expect_equal(x2(line),zero)
  expect_equal(y2(line),zero)
  expect_is(line,"SVGShape")
  expect_is(line,"Vector")
  
  id(line) <- "line1"
  bbox(line) <- list(x1="10px",y1="20px",x2="100px",y2="70px")
  cssClass(line) <- "line.style"
  cssStyle(line) <- "stroke:blue; stroke-width:2pt"
  svgTransform(line) <- "translate(5,6)"
  
  expect_equal(id(line), "line1")
  expect_equal(svgTransform(line),"translate(5,6)")
  expect_equal(cssClass(line),"line.style")
  expect_equal(cssStyle(line),"stroke:blue; stroke-width:2pt")
  expect_equal(x1(line),SVGCoord.factory("10px"))
  expect_equal(y1(line),SVGCoord.factory("20px"))
  expect_equal(x2(line),SVGCoord.factory("100px"))
  expect_equal(y2(line),SVGCoord.factory("70px"))
  expect_is(line,"SVGShape")
  expect_is(line,"Vector")
  
})

test_that("Rendering", {
  
  line <- SVGLine.factory(bbox=list(x1="10px",y1="20px",x2="100px",y2="70px"),
                          class="line.style",
                          style="stroke:blue; stroke-width:2pt",
                          transform="translate(5,6)")
  id(line) <- "line1"
  expect_output(.xml(line),"<line id=\"line1\" transform=\"translate\\(5,6\\)\" class=\"line.style\" style=\"stroke:blue; stroke-width:2pt\" x1=\"10px\" y1=\"20px\" x2=\"100px\" y2=\"70px\"/>.*")
  
})