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
    pak::pkg_system_requirements,
    os = "ubuntu", os_release = "20.04"
  ) %>%
    purrr::set_names(pkgs) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    stringr::str_replace_all(., "apt-get install -y ", "")
}

#' Get system requirements by R package using Remotes
#'
#' @param packages character vector of packages names
#' @param quiet boolean if TRUE the function is quiet
#' @param batch_n number of simultaneous packages to ask
#'
#' @return character vector of sysreqs
#' @export
#'
#' @importFrom remotes package_deps
get_sysreqs_remotes <- function(packages, quiet = TRUE, batch_n = 30) {
  all_deps <- sort(unique(c(packages, unlist(remotes::package_deps(packages)$package))))

  sp <- split(all_deps, ceiling(seq_along(all_deps) / batch_n))

  hold <- lapply(sp, function(.x) {
    get_batch_sysreqs(.x, quiet = quiet)
  }) %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()

  setdiff(hold, sysreqs_in_base)
}

#' @keywords internal
#' @noRd
#' @importFrom fs file_temp file_delete
#' @importFrom jsonlite fromJSON
#' @importFrom utils download.file
get_batch_sysreqs <- function(all_deps, quiet = TRUE) {
  url <- sprintf(
    "https://sysreqs.r-hub.io/pkg/%s/linux-x86_64-debian-gcc",
    paste(all_deps, collapse = ",")
  )

  path <- fs::file_temp()

  utils::download.file(url, path, mode = "wb", quiet = quiet)

  out <- jsonlite::fromJSON(path)

  fs::file_delete(path)

  unique(out[!is.na(out)])
}

sysreqs_in_base <- c(
  "gdebi-core",
  "git-core",
  "libcurl4-gnutls-dev",
  "wget"
)

# get_sysreqs("inst/testapp")
