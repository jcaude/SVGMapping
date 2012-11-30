require(testthat)

context("Mask")

test_that("Accessors", {
  
  # init.
  mask <- Mask.factory()
  udef <- SVGCoord.factory(-10,"%")
  ldef <- SVGLength.factory(120,"%")
  
  # check default
  expect_equal(x(mask),udef)
  expect_equal(y(mask),udef)
  expect_equal(width(mask),ldef)
  expect_equal(height(mask),ldef)
  expect_equal(maskUnits(mask),"objectBoundingBox")
  expect_equal(maskContentUnits(mask),"userSpaceOnUse")
  expect_equal(maskContent(mask),NULL)
  expect_is(mask,c("SVGNode","Rectangle"))
  
  # set various fields
  x <- SVGCoord.factory(-4.3,"mm")
  y <- SVGCoord.factory(2.1,"cm")
  w <- SVGLength.factory(43,"in")
  h <- SVGLength.factory(135,"%")
  r <- SVGRect.factory("1cm","1cm","100px","150px")
  
  x(mask) <- x
  y(mask) <- y
  width(mask) <- w
  height(mask) <- h
  maskUnits(mask) <- "userSpaceOnUse"
  maskContentUnits(mask) <- "objectBoundingBox"
  maskContent(mask) <- .xml(r)
  
  # check new sets
  expect_equal(x(mask),x)
  expect_equal(y(mask),y)
  expect_equal(width(mask),w)
  expect_equal(height(mask),h)
  expect_equal(maskUnits(mask),"userSpaceOnUse")
  expect_equal(maskContentUnits(mask),"objectBoundingBox")
  expect_output(print(maskContent(mask)), "<rect x=\"1cm\" y=\"1cm\" width=\"100px\" height=\"150px\"/>")
  
  # fouls setter
  expect_error(maskUnits(mask) <- "foo")
  expect_error(maskContentUnits(mask) <- "bar")
  expect_error(maskContent(mask) <- list(1,2,3))
  
})

test_that("Factory", {
  
  # default factory
  mask <- Mask.factory()
  expect_output(print(.xml(mask)),"<mask/>")
  
})