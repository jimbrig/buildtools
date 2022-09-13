#' Get sysreqs generic
#'
#' Retrieve R Package system requirements using a suite of various methods.
#'
#' @param pkgs Character vector of R packages to check for.
#' @param platform Defaults to "linux-x86_64-debian-gcc" for Docker images.
#' @param method Method to use. Should be one of pak, dep, rhub, rstudio, sysreqs.
#' @param ... For methods
#'
#' @keywords sysreqs
#' @seealso [dep::get_sysreqs()], [pak::pkg_system_requirements()]
#'
#' @return Character vector of system requirements associated with the supplied packages.
#' @export
#'
#' @examples
#' pkgs <- c("magick", "tinytex")
#' get_sysreqs(pkgs, method = "rhub")
get_sysreqs <- function(pkgs, platform = NULL, method = NULL, ...) {

  if (!is.null(method)) method <- match.arg(method, c("rhub", "rstudio", "pak", "dep", "sysreqs"))

  class(pkgs) <- c(method, class(pkgs))

  UseMethod("get_sysreqs", pkgs, ...)

}

#' @export
#' @describeIn get_sysreqs Default Method: This method uses the `sysreqs` package
#'   directly by calling the function [sysreqs::sysreqs()] and using package
#'   `DESCRIPTION` files.
#' @note Any packages supplied should be installed on local system to allow this
#' function to work properly.
#' @importFrom desc description
#' @importFrom fs path
get_sysreqs.default <- function(pkgs, ...) {
  dsc <- desc::description$new(fs::path(.libPaths()[1], pkg, "DESCRIPTION"))
  sysreqs:::get_sysreqs(dsc$get("SystemRequirements"), platform = "r-release-linux-x86_64")
}


#' @describeIn get_sysreqs `dep` Method: This method uses [dep::get_sysreqs()]
#'   to retrieve system dependencies.
#' @export
#' @importFrom dep get_sysreqs
get_sysreqs.dep <- function(pkgs, ...) {

  dep::get_sysreqs(pkgs)

}


#' @describeIn get_sysreqs R-Hub API Method: This method of the `get_sysreqs`
#'   generic performs a `GET` request to the R-Hub SysReqs API at path:
#'   `https://sysreqs.r-hub.io/pkg/<pkg1>,<pkg2>,<pkg3>/<platform>` using
#'   [httr::GET()].
#' @export
#' @importFrom httr GET content
get_sysreqs.rhub <- function(pkgs = NULL, platform = "linux-x86_64-debian-gcc") {

  if (is.null(pkgs)) return(NULL)
  base_url <- "https://sysreqs.r-hub.io/pkg/"
  uri <- paste0(base_url, paste(pkgs, collapse = ","), "/", platform)

  uri %>%
    httr::GET() %>%
    httr::content(type = 'application/json', as = 'parsed') %>%
    unlist()

}

#' @describeIn get_sysreqs `pak` Method: This method wraps [pak::pkg_system_requirements()]
#'   to retrieve R package system requirements.
#' @export
#' @importFrom pak pkg_system_requirements
#' @importFrom purrr map_chr
get_sysreqs.pak <- function(pkgs, ...) {
  purrr::map_chr(pkgs, pak::pkg_system_requirements, os = "ubuntu", os_release = "20.04")
}

