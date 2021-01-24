
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


# edit DESCRIPTION --------------------------------------------------------


