# hotfun 0.1.8

- Updated `tbl_propdiff()` so that unadjusted and adjusted differences are presented as the rate in the right column subtracted from the rate in the left column to match results of `bstfun::tbl_ancova`
- Added `label` and `statistic` arguments to `tbl_propdiff`

# hotfun 0.1.7

- Updated `tbl_propdiff()` to give chi-squared or Fisher's exact p-value, and fixed issue caused by having logical `"x"` variable.

# hotfun 0.1.6

- Removed {bstfun} dependency 

- Removed {gt} dependency 

# hotfun 0.1.5

- Added `assign_timepoint()` function

- Updated `count_map()` to sort by variables rather than frequency

# hotfun 0.1.4

- Added Appveyor CI checks, and added addition Travis CI checks

- Updated `count_map()` to no longer print row numbers

- Updated `tbl_anova()` to use NS `tbl_summary_()`

# hotfun 0.1.3

- Updated output for `count_map()` and `count_na()`

# hotfun 0.1.2

- Added `count_map()` and `count_na()`

- Added gt as Depends package

# hotfun 0.1.1

- Fix for `tbl_ancova()`. The formula argument was hard-coded to "{y} ~ {x}", rather than using the user supplied `formula =`.

# hotfun 0.1.0

- first release
