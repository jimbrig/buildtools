
#  ------------------------------------------------------------------------
#
# Title : buildtools R Package development
#    By : Jimmy Briggs
#  Date : 2020-12-29
#
#  ------------------------------------------------------------------------

# library dev pkgs --------------------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(
  devtools,
  usethis,
  pkgbuild,
  pkgload,
  codetools,
  goodpractice,
  roxygen2,
  desc,
  attachment
)

# check sysreqs -----------------------------------------------------------
devtools::has_devel()


# create package ----------------------------------------------------------

usethis::create_package("buildtools")

# setup directories
dirs <- c(
  "inst/scripts",
  "data-raw",
  "data",
  "inst/images",
  "inst/extdata",
  "inst/templates",
  "vignettes"
)

purrr::walk(dirs, fs::dir_create, recurse = TRUE)
rm(dirs)

# setup namespace and roxygen
usethis::use_namespace()
usethis::use_roxygen_md()

# package R documentation and basic imports
usethis::use_package_doc()
usethis::use_tibble() # #' @return a [tibble][tibble::tibble-package]
usethis::use_pipe() # move to propaloc-package.R
usethis::use_tidy_eval() # move to propalloc-package.R

# initial document
devtools::document()

# set title and description for GH before making GH project
desc::desc_set(Title = "buildtools R Package",
               Description = "Tools for isolating, detecting dependencies,
               scaffolding, and building deployable, reproducible developer environments in R.")

devtools::document()

# git and github ----------------------------------------------------------

usethis::use_git()
usethis::use_github(private = TRUE)
usethis::git_vaccinate()
usethis::git_sitrep()


# edit DESCRIPTION --------------------------------------------------------

# version
usethis::use_dev_version()

# license
usethis::use_mit_license()

# normalize
desc::desc_normalize()


# README ------------------------------------------------------------------

usethis::use_readme_rmd()
# usethis::use_logo("inst/images/logo.png")
usethis::use_lifecycle_badge("Experimental")
usethis::use_badge(
  "Project Status: WIP",
  href = "http://www.repostatus.org/#wip",
  src = "https://www.repostatus.org/badges/latest/wip.svg"
)
knitr::knit("README.Rmd")


# data-raw ----------------------------------------------------------------


# functions ---------------------------------------------------------------
c(
  "build",
  "deploy",
  "docker",
  "sysreqs",
  "pkgdeps",
  "logging",
  "meta",
  "cache",
  "utils"
) |> purrr::walk(usethis::use_r, open = FALSE)


# documentation -----------------------------------------------------------

usethis::use_news_md()


# vignettes ---------------------------------------------------------------

usethis::use_vignette("buildtools")
usethis::use_vignette("sysreqs")


# tests -------------------------------------------------------------------
usethis::use_testthat()
usethis::use_test("test")

# coverage ----------------------------------------------------------------

usethis::use_coverage()
usethis::use_github_action("test-coverage")


# changelog ---------------------------------------------------------------

fs::file_create("cliff.toml")
system("git-cliff -i")
system("git-cliff -o inst/CHANGELOG.md")
system("git-cliff -o NEWS.md")

# build -------------------------------------------------------------------

# check build tools
pkgbuild::check_build_tools()
devtools::dev_sitrep()

# update devt packages
rstudioapi::restartSession()
devtools::update_packages("devtools")

# knitr README
knitr::knit("README.Rmd")

# update dependencies
# detach_packages() # get_deps()
rstudioapi::restartSession()
attachment::att_amend_desc(
  extra.suggests = c("attempt", "devtools", "attachment", "roxygen2")
)
attachment::create_dependencies_file(to = "inst/scripts/dependencies.R")

# document
devtools::document()

# check & test
devtools::check()
devtools::test()

# goodpractice check
goodpractice::gp()

# install
devtools::install()

# builds
devtools::build()
devtools::build_vignettes()
devtools::build_manual()

# release
spelling::update_wordlist()
devtools::spell_check()
devtools::release()

# R CMD CHECK RESULTS -----------------------------------------------------
#   Duration: 1m 48s
#
# 0 errors v | 0 warnings v | 0 notes v
#
# R CMD check succeeded


# extras ------------------------------------------------------------------

# usethis::use_coverage()
covr::package_coverage()

knitr::knit("README.Rmd")
