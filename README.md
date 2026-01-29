
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vocomatcher

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/jaytohe/ecostats-rshiny/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jaytohe/ecostats-rshiny?branch=main)
<!-- badges: end -->

The goal of vocomatcher is to assist with matching and deduplicating
animal calls in microphone array based acoustic surveys.

This repo originally forked from https://github.com/jaytohe/ecostats-rshiny.git.

## R Version

The app has only been tested on R version 4.4.1 (2024-06-14).

## Installation

You can install the development version of vocomatcher from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iandurbach/ecostats-rshiny")
```

If you want to try vocomatcher with the example data provided under
`data/poc_spectro`, there are two options:

## Option 1

1.  `git clone https://github.com/iandurbach/ecostats-rshiny.git`
2.  Open up RStudio in the directory of the cloned repository
3.  Install the `golem` package
4.  Open `dev/01_start.R` and run it.
5.  Open `dev/02_start.R` and run it.
6.  Open `dev/run_dev.R` and run it.
7.  This should launch the Shiny app on your browser.
8.  From Vocomatcher, in the Import CSVs screen, you can then navigate
    to the directory of the cloned repo and find the example data under
    `data/poc_spectro`.
9.  In said directory, `mic.csv` is the microphone location coordinates
    csv file, `recordings.csv` is the recorded calls csv.

## Option 2

1.  Open up an R terminal and type:

``` r
# install.packages("devtools")
devtools::install_github("iandurbach/ecostats-rshiny")
```

This should install all the dependencies of vocomatcher and vocomatcher
itself.

2.  Clone the repo (from a regular shell):

`git clone https://github.com/iandurbach/ecostats-rshiny.git`

3.  Back on the R terminal, run the Shiny app using:
    `vocomatcher::run_app()`

4.  Follow steps 8, 9 from Option 1.

*Note: The example data is not real.*
