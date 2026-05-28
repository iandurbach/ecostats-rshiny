# Minimal launcher for deployment that loads the bundled package source
# Minimal launcher for deployment that loads the bundled package source
if (!requireNamespace("pkgload", quietly = TRUE)) {
  install.packages("pkgload")
}

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Raise upload limit (default 5 MB) to allow spectrogram ZIP uploads
options(shiny.maxRequestSize = 100 * 1024^2) # 100 MB
options(golem.app.prod = TRUE)

vocomatcher::run_app()
