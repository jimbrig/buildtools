#' Get System Requirements for R Packages
#'
#' @param pkgs R package dependencies as a character vector
#'
#' @return character vector
#' @export
#'
#' @examples \dontrun{
#' get_sysreqs("tidyverse")
#' get_sysreqs(c("plumber", "rmarkdown"))
#' }
get_sysreqs <- function(pkgs) {

  purrr::map(
    pkgs,
    pak::pkg_system_requirements, os = "ubuntu", os_release = "20.04"
  ) %>%
    purrr::set_names(pkgs) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    stringr::str_replace_all(., "apt-get install -y ", "")

}
