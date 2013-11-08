require(testthat)

context("SVGTransform")

test_that("Accessors",{
  
  # init.
  tf <- SVGTransform.factory()
  
  # check default
  expect_equal(transforms(tf),character(0))
  expect_equal(ctm(tf),matrix(0,nrow=0,ncol=0))
  
  # set various fields
  transforms(tf) <- "translate(50,90), rotate(-45)  translate(130 , 160 )"
  update(tf) <- diag(3)
  
  # check new sets
  expect_equal(length(transforms(tf)),3)
  expect_equal(transforms(tf)[1],"translate(50,90)")
  expect_equal(transforms(tf)[2],"rotate(-45)")
  expect_equal(transforms(tf)[3],"translate(130 , 160 )")
  expect_equal(ctm(tf), matrix(c(0.7071068,-0.7071068,0.0000000,
                                 0.7071068,0.7071068,0.0000000,
                                 255.0609665,111.2132034,1.0000000),
                               nrow=3,ncol=3))
  
  # fools setter
  expect_error(transforms(tf) <- 2)
  expect_error(transforms(tf) <- "foo(x)")
  expect_error(transforms(tf) <- "svg is the future")
  expect_error(update(tf) <- 1)
  expect_error(update(tf) <- "2")
  expect_error(update(tf) <- diag(4))
  
})

test_that("Factory", {
  
  # default factory
  tf <- SVGTransform.factory()
  
  # check
  expect_equal(as.character(tf),character(0))
  
  # full factory
  ops <- "translate(50,90), rotate(-45) , translate(130 , 160 )"
  fix.ops <- "translate(50,90) rotate(-45) translate(130 , 160 )"
  tf <- SVGTransform.factory(ops)
  
  # check
  expect_equal(as.character(tf), fix.ops)
})