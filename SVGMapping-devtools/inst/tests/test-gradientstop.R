context("Gradient Stop")

test_that("Factory, get/set", {
  
  myStop <- GradientStop.factory()
  
  expect_identical(offset(myStop), "0")
  expect_identical(color(myStop), "white")
  expect_equal(opacity(myStop), 1)
  expect_is(myStop,"GradientStop")
  
  offset(myStop) <- 0.5
  color(myStop) <- "red"
  opacity(myStop) <- 00.90000000000000000000000000001
  
  expect_identical(offset(myStop), "0.5")
  expect_identical(color(myStop), "red")
  expect_equal(opacity(myStop), 0.9)
}
)

test_that("XML conversion", {
  
  myStop <- GradientStop.factory(0.4,"yellow")
  
  expect_output(print(.xml(myStop)),'<stop offset="0.4" stop-color="yellow" stop-opacity="1"/>')
})