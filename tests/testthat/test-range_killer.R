test_that("Range_killer returns some values correctly", {
  res <- range_killer(m=c("2.0"), s=c("1.0"), n=c(50), min=c("0.5"), max=c("3.5"))
  expect_equal(round(res$Min_mean, 5), round(1.945833, 5))

}
)

test_that("Range_killer output behaves correctly", {
  expect_output(range_killer(m=c("2.0"), s=c("1.0"), n=c(50), min=c("0.5"), max=c("3.5"), output = TRUE))

}
)
