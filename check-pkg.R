# ---- Documentation ----
options(install.opts = list(roxygen2 = "--no-multiarch"))
example(source)
try(roxygen2::roxygenize(load_code = sourceDir,
                         clean = TRUE),
    silent = TRUE
)
pkgbuild::compile_dll()
roxygen2::roxygenize()

usethis::use_testthat()
devtools::document()

# ---- Checks ----
devtools::check()
