library(rarsim)

simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 20), rpois(1000, 10), rpois(1000, 5)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("reward lists all have the same names when no names are given", {

  expect_equal(
    names(scheduler@rewards),
    names(simulator@draw)
  )

  expect_equal(
    names(scheduler@rewards),
    names(simulator@pool)
  )

  expect_equal(
    names(scheduler@rewards),
    names(getPermutations(scheduler)@rewards)
  )

  expect_equal(
    names(scheduler@rewards),
    NULL
  )
})

simulator <- simulator.start(pool = list("A" = rpois(1000, 10), "B" = rpois(1000, 20), "C" = rpois(1000, 10), "D" = rpois(1000, 5)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("reward lists all have the same names when names are given", {

  expect_equal(
    names(scheduler@rewards),
    names(simulator@draw)
  )

  expect_equal(
    names(scheduler@rewards),
    names(simulator@pool)
  )

  expect_equal(
    names(scheduler@rewards),
    names(getPermutations(scheduler)@rewards)
  )

  expect_equal(
    names(scheduler@rewards),
    c("A", "B", "C", "D")
  )
})
