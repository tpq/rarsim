library(rarsim)

rewards <- list(rep(1, 10), rep(2, 20), rep(3, 20), rep(4, 20))

test_that("permute methods work correctly", {

  expect_equal(
    rewards,
    permute.from.null(rewards)
  )

  expect_false(
    identical(rewards, permute.from.all(rewards))
  )

  expect_true(
    all(permute.from.control(rewards, 1)[[2]] == 1)
  )

  expect_true(
    all(permute.from.control.norm(rewards, 1)[[2]] == 1)
  )
})

simulator <- simulator.start(pool = list(rpois(1000, 10), rpois(1000, 20), rpois(1000, 10), rpois(1000, 5)))
scheduler <- scheduler.start(rep(4, 4), rep(Inf, 4), N.burn.in = 20)
for(trial in 1:5){
  simulator <- simulator.draw(simulator, getAllocation(scheduler))
  scheduler <- scheduler.update(scheduler, getDraw(simulator), N.allocate = 20)
}

test_that("getPermutations passes permute method correctly", {

  expect_equal(
    scheduler@rewards,
    getPermutations(scheduler, how = permute.from.null)@rewards
  )

  expect_equal(
    scheduler@post.mean,
    getPermutations(scheduler, how = permute.from.null)@post.mean
  )
})
