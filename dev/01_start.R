# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Ensure required scaffolding exists ----
## {golem} rewrites the vignette when setting the package name; if the package
## name hasn't changed the default helper removes the file. Create it as needed
## before and after the operations that touch it.
vignette_path <- file.path("vignettes", "vocomatcher.Rmd")
vignette_template <- c(
  "---",
  'title: "VocoMatcher"',
  "output: rmarkdown::html_vignette",
  "vignette: >",
  "  %\\VignetteIndexEntry{VocoMatcher}",
  "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}",
  "---",
  "",
  "```{r, include = FALSE}",
  "knitr::opts_chunk$set(",
  "  collapse = TRUE,",
  '  comment = "#>"',
  ")",
  "```",
  "",
  "```{r setup}",
  "library(vocomatcher)",
  "```"
)

ensure_vignette <- function() {
  dir.create(dirname(vignette_path), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(vignette_path)) {
    writeLines(vignette_template, vignette_path)
  }
}

ensure_vignette()

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "vocomatcher", # The Name of the package containing the App
  pkg_title = "VocoMatcher", # The Title of the package containing the App
  pkg_description = "A Shiny app to visually match animal vocalizations in microphone-array PAM studies.", # The Description of the package containing the App
  authors = person(
    given = "Anthony",
    family = "Dalamagas",
    email = "anthony.dalamagas@gmail.com",
    role = c("aut", "cre")
  ),
  repo_url = "https://github.com/jaytohe/ecostats-rshiny", # The URL of the GitHub Repo (optional),
  pkg_version = "0.0.0.9000", # The Version of the package containing the App
  set_options = FALSE
)

## The call above removes vignettes/vocomatcher.Rmd when the name is unchanged
## so recreate it before continuing.
ensure_vignette()

## Set {golem} options ----
golem::set_golem_options()
ensure_vignette()

## Install the required dev dependencies ----
if (requireNamespace("attachment", quietly = TRUE)) {
  golem::install_dev_deps()
} else {
  warning("Skipping golem::install_dev_deps(): the {attachment} package is not installed.")
}

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("jaytohe (Anthony Dalamagas)") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
tryCatch(
  devtools::build_readme(),
  error = function(e) warning("Skipping devtools::build_readme(): ", conditionMessage(e))
)
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "jaytohe (Anthony Dalamagas)")
usethis::use_lifecycle_badge("Experimental")
#usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
if (requireNamespace("processx", quietly = TRUE)) {
  golem::use_recommended_tests()
} else {
  warning("Skipping golem::use_recommended_tests(): the {processx} package is not installed.")
}

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::navigateToFile("dev/02_dev.R")
} else {
  message("RStudio not available; open dev/02_dev.R manually if needed.")
}
