# Golem -------------------------------------------------------------------

if (FALSE) {
    golem::create_golem(
        path = "dbt-def-shinymods",
        check_name = FALSE,
        open = FALSE,
        overwrite = TRUE,
        with_git = TRUE
    )
}

# Dev dependencies: install -----------------------------------------------

# renv::activate()

renv::install("devtools")
renv::install("testthat")
renv::install("roxygen2")
renv::install("roxygen2md")
renv::install("rmarkdown")
renv::install("here")


if (FALSE) {
    renv::install("attachment")
    renv::install("yonicd/covrpage")
    renv::install("rhub")
    usethis::use_package("yonicd/covrpage", type = "Suggests")
    usethis::use_package("rhub", type = "Suggests")
    usethis::use_package("attachment", type = "Suggests")
}

# Dev dependencies: declare -----------------------------------------------

usethis::use_package("devtools", type = "Suggests")
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("roxygen2", type = "Suggests")
usethis::use_package("roxygen2md", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("here", type = "Suggests")

# Dev preps ---------------------------------------------------------------

# "Add the pipe"
usethis::use_pipe()

# Add package description
usethis::use_package_doc(open = FALSE)

# Use {testthat}
usethis::use_testthat()

# Use markdown in roxygen syntax
usethis::use_roxygen_md()
roxygen2md::roxygen2md()

# Covered in 'dev/01_start.R':
# usethis::use_mit_license()
# usethis::use_readme_rmd()
usethis::use_lifecycle()
# usethis::use_lifecycle_badge("experimental")
# usethis::use_news_md(open = FALSE)

usethis::use_build_ignore(
    c(
        "dev",
        "inst/examples",
        "tests"
    )
)

# Prod dependencies: install ----------------------------------------------

renv::install("shinyjs")
# renv::install("shinydashboardPlus", rebuild = TRUE)
renv::install("RInterface/shinydashboardPlus", rebuild = TRUE)
renv::install("shinyWidgets")

renv::install("dplyr")
renv::install("DT")

renv::install("rappster/wrang", rebuild = TRUE)
renv::install("rappster/drop", rebuild = TRUE)
renv::install("rappster/dtf", rebuild = TRUE)

# Prod dependencies: declare ----------------------------------------------

usethis::use_package("shinyjs")
# usethis::use_package("shinydashboardPlus")
usethis::use_package("shinyWidgets")
usethis::use_package("dplyr")
usethis::use_package("DT")
usethis::use_dev_package("wrang", type = "Imports", remote = "rappster/wrang")
usethis::use_dev_package("drop", type = "Imports", remote = "rappster/drop")
usethis::use_dev_package("dtf", type = "Imports", remote = "rappster/dtf")
usethis::use_dev_package("shinydashboardPlus", type = "Imports", remote = "RInterface/shinydashboardPlus")

# Continuous dev ----------------------------------------------------------

usethis::use_version("dev")

