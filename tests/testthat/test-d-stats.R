library(rarsim)

set.seed(1)
simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 10), rpois(1000, 10), rpois(1000, 10)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20, sampler = thompson)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("p-values are greater than .05 for the null condition", {

  expect_true(
    stats.anova(scheduler) > .05
  )

  expect_true(
    all(stats.ttest(scheduler, 1) > .05)
  )

  expect_true(
    all(stats.ttest(scheduler, 1) > .05)
  )
})

set.seed(1)
simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 20), rpois(1000, 20), rpois(1000, 20)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20, sampler = thompson)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("p-values are less than .05 for the real deal", {

  expect_true(
    stats.anova(scheduler) < .05
  )

  expect_true(
    all(stats.ttest(scheduler, 1) < .05)
  )

  expect_true(
    all(stats.ttest(scheduler, 1) < .05)
  )
})
