## rarsim 0.0.8
---------------------
* Launch unknown variance update
    * Add `prior.nu` and `post.nu` slots
    * Add prior nu, alpha, and beta arguments to `scheduler.start` function
    * Add procedure to guess prior nu, alpha, and beta from prior var
    * Make `sampler.auc` use t-distribution too
* Pass arguments to `sampler` function
    * Add `sampler.auc.cutoff` to allocate patients based on p(sample > cutoff)
    * Add `sampler.auc.reference` to allocate patients based on p(sample > reference)
    * The `scheduler.start` will save `...` args to `sampler.args` slot
    * These get passed to the `sampler` during an update

## rarsim 0.0.7
---------------------
* Prepare unknown variance update
    * Remove `prior.prec` and `post.prec` slots for simplicity
    * Add slots to store degrees-of-freedom (set to Inf when heuristic = TRUE)
    * Replace Normal distribution with t distribution
* Update `run.benchmark` method
    * New `fast` argument to measure p-value at final time step only

## rarsim 0.0.6
---------------------
* Add `sampler` methods
    * New `sampler.fixed` will allocate patients at a fixed ratio
    * New `sampler.auc` will allocate patients proportional to p(posterior > cutoff)
* Prepare unknown variance update
    * Add `prior.alpha` and `prior.beta` slots to anticipate normal-gamma conjugate
    * (`prior.var` and `prior.prec` save marginal values for backwards compatibility)
    * Add `post.alpha` and `post.beta` slots to anticipate normal-gamma conjugate
    * (`post.var` and `post.prec` save marginal values for backwards compatibility)
    * Add `heuristic` argument to toggle normal-gamma use

## rarsim 0.0.5
---------------------
* Improvements
    * Have `scheduler` object save name of sampler method
    * New `run.trial` iterates `simulator.draw` and `scheduler.update` steps
    * New `run.benchmark` estimates proportion of (p < alpha)
    * Name all sampler methods with `sampler.` prefix

## rarsim 0.0.4
---------------------
* Improvements
    * Consistently name reward lists
    * New `stats.anova` computes a p-value from the rewards
    * New `stats.ttest` computes p-values from the rewards
    * New `stats.empiric` computes empiric p-values

## rarsim 0.0.3
---------------------
* Get ready for statistics modules
    * Add empty `@meta` slot for future updates
    * New `getDivergences` function can calculate divergence between two posteriors
    * New `getPermutations` function can permute rewards in `scheduler` object

## rarsim 0.0.2
---------------------
* Improvements
    * Add first edition of README for collaborators
* Bug fixes
    * Fix error caused by trying to name `simulator` rewards

## rarsim 0.0.1
---------------------
* Create S4 `scheduler` class for scheduling trials
    * This object stores past rewards, updates all posteriors, and recommends new allocations
    * Key function `scheduler.start` creates a new `scheduler` object
    * Key function `scheduler.update` updates a `scheduler` object with new data
    * Add some getters and basic plots
* Create S4 `simulator` class for simulating data
    * This object stores a pool of all data and the most recent draw from this pool
    * Key function `simulator.start` creates a new `simulator` object
    * Key function `simulator.draw` draws data from a `simulator` object
    * Add some getters and setters
* Design notes
    * The `scheduler` decides on the allocation, which is passed to the `simulator`
    * The `simulator` draws patient data from the pool, which is passed back to the `scheduler`
    * This process is repeated until the analyst is satisfied
