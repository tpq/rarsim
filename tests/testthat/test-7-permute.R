library(rarsim)
rewards <- list(rep(1, 10), rep(2, 20), rep(3, 20), rep(4, 20))

test_that("permute methods work correctly", {

  expect_equal(
    rewards,
    permute.null(rewards)
  )

  expect_false(
    identical(rewards, permute.all(rewards))
  )

  expect_true(
    all(permute.control(rewards, 1)[[2]] == 1)
  )
})

set.seed(3220)
simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 20), rpois(1000, 10), rpois(1000, 5)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20, sampler = thompson)
for(trial in 1:5){
  request <- getAllocation(scheduler)
  simulator <- simulator.draw(simulator, request)
  rewards <- getDraw(simulator)
  scheduler <- scheduler.update(scheduler, rewards, N.allocate = 20)
}

test_that("getPermutations passes permute method correctly", {

  expect_equal(
    scheduler@rewards,
    getPermutations(scheduler, how = permute.null)@rewards
  )

  expect_equal(
    scheduler@post.mean,
    getPermutations(scheduler, how = permute.null)@post.mean
  )
})
