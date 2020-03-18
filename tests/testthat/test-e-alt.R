library(rarsim)

test_that("alternative distribution functions work as expected", {

  set.seed(1)
  a <- rnorm(1001, mean = 3, sd = 4)
  set.seed(1)
  b <- alt_rnorm(1001, mean = 3, sd = 4)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- pnorm(1:100, mean = 3, sd = 4)
  set.seed(1)
  b <- alt_pnorm(1:100, mean = 3, sd = 4)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- qnorm(c(.1, .2, .3, .4, .5), mean = 3, sd = 4)
  set.seed(1)
  b <- alt_qnorm(c(.1, .2, .3, .4, .5), mean = 3, sd = 4)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- dnorm(c(.1, .2, .3, .4, .5), mean = 3, sd = 4)
  set.seed(1)
  b <- alt_dnorm(c(.1, .2, .3, .4, .5), mean = 3, sd = 4)
  expect_equal(
    cor(a, b),
    1
  )

  set.seed(1)
  a <- rt(1001, df = 3)
  set.seed(1)
  b <- alt_rt(1001, df = 3)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- pt(1001, df = 3)
  set.seed(1)
  b <- alt_pt(1001, df = 3)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- qt(c(.1, .2, .3, .4, .5), df = 3)
  set.seed(1)
  b <- alt_qt(c(.1, .2, .3, .4, .5), df = 3)
  expect_equal(
    a,
    b
  )

  set.seed(1)
  a <- dt(c(.1, .2, .3, .4, .5), df = 3)
  set.seed(1)
  b <- alt_dt(c(.1, .2, .3, .4, .5), df = 3)
  expect_equal(
    cor(a, b),
    1
  )
})
