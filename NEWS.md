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
