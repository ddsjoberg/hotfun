# hotfun 0.2.2

* Updated `Remotes:` to track the release versions of starter and rstudio.prefs

# hotfun 0.2.1

* Added an `.Rprofile` to the project template.

# hotfun 0.2.0

* Updates to `tbl_propdiff()` for the gtsummary v1.4.0 updates.

* Added new function `create_hot_project()`, a wrapper for `starter::create_project()` that drops a copy of the HOT project template into a new or existing folder. The function defaults to the HOT template, but any template may be passed.

* Added new functions `use_hot_file()`, `use_hot_gitignore()`, and `use_hot_readme()` to drop files from the HOT template into the active project.

* Migrated the HOT template from the hotverse package (behind a firewall) to the hotfun package.

* Added new function `use_hot_rstudio_prefs()`, a wrapper for `rstudio.prefs::use_rstudio_prefs()` that is pre-filled with options that help fulfill best practices.

# hotfun 0.1.12

- Fixed issue with reversing factor levels for multivariable methods with no covariates specified in `tbl_propdiff`

# hotfun 0.1.11

- Fixed issue with calculation of adjusted difference when reversing factor levels in `tbl_propdiff`

# hotfun 0.1.10

- Added error message to `add_splines()` when new variable names already exist in data frame.

# hotfun 0.1.9

- Added function `add_splines()` to calculate spline terms and attach the spline terms and knots to the original data

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
