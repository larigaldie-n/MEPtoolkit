test_that("anova_one_descriptive returns some values correctly", {
  res <- anova_one_descriptive(m = c("2.51", "3.12"), s = c("0.58", "0.34"), n = c(40, 40))
  expect_equal(round(res$Min, 5),
               round(9.168941e-08, 5))
  expect_equal(round(res$RIVETS, 5),
               round(1.742168e-07, 5))
  expect_equal(round(res$Max, 5),
               round(3.249061e-07, 5))
  expect_equal(res$Function,
               "anova_one_descriptive")
}
)

test_that("anova_one_descriptive output behaves normally", {
  expect_output(anova_one_descriptive(m = c("2.51", "3.12"), s = c("0.58", "0.34"), n = c(40, 40), output = TRUE))
}
)
