library(rarsim)

simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 20), rpois(1000, 10), rpois(1000, 5)))
scheduler <- scheduler.start(rep(0, 4), rep(1, 4), N.burn.in = 20)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("divergences make sense", {

  expect_equal(
    class(getDivergences(scheduler)),
    "matrix"
  )

  expect_equal(
    diag(getDivergences(scheduler)),
    c(0, 0, 0, 0)
  )

  kl <- getDivergences(scheduler, how = rarsim::diverge.kl)
  expect_equal(
    getDivergences(scheduler),
    (kl + t(kl))/2
  )
})
