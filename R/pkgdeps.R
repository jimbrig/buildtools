#' Get Package Dependencies
#'
#' @param app_dir directory to scan
#' @param ignores packages to ignore (i.e. remotes)
#' @param verbose logical
#'
#' @return list of package dependencies
#' @export
#'
#' @importFrom cli cli_alert_warning cli_alert_danger
#' @importFrom glue glue
#' @importFrom purrr safely map_depth pluck compact map
get_pkgdeps <- function(app_dir = "shiny_app", ignores = NULL, verbose = TRUE) {
  if (!dir.exists(app_dir)) stop(glue::glue("Invalid `app_dir` argument. `{app_dir}` does not exist."))
  init_pkg_names <- get_dependent_packages(app_dir)
  if (length(init_pkg_names) == 0) {
    cli::cli_alert_warning("warning: no packages found in specified directory")
    return(invisible(NULL))
  }
  hold <- suppressWarnings(
    lapply(init_pkg_names, purrr::safely(get_package_details, quiet = TRUE))
  )
  names(hold) <- init_pkg_names
  errors <- purrr::map_depth(hold, 1, purrr::pluck, "error") %>%
    purrr::compact() %>%
    names()
  hold <- hold[!(names(hold) %in% errors)]
  if (length(errors) > 0 && verbose) {
    cli::cli_alert_danger("Silently removing detected invalid packages: {msg_value(errors)}")
  }

  deps <- purrr::map_depth(hold, 1, purrr::pluck, "result") %>%
    purrr::map(function(x) {
      if (length(x) == 0) {
        return(NULL)
      } else {
        return(x)
      }
    }) %>%
    purrr::compact()

  deps[!(names(deps) %in% c(ignores, "remotes"))]
}

#' Create a `deps.yml` Package Dependency File
#'
#' @description
#' This function creates a specifically structured `YAML` file to house a Shiny
#' App's dependencies.
#'
#' @param deps list returned from [get_pkgdeps()]; leave null to run [get_pkgdeps()].
#' @param app_dir the application's path
#' @inheritDotParams get_pkgdeps
#'
#' @return unnamed list of packages
#' @export
#'
#' @importFrom yaml write_yaml
create_pkgdeps_file <- function(deps = NULL, app_dir = "shiny_app", ...) {
  if (is.null(deps)) {
    msg_info("Retrieving package dependencies from {msg_path(app_dir)}...")
    deps <- get_pkgdeps(app_dir = app_dir, ...)
    msg_info("Found {length(deps)} package dependencies.")
  }
  yaml::write_yaml(deps, file.path(app_dir, "deps.yml"))
  msg_done("Successfully created `deps.yml` for app at {msg_path(app_dir)}.")
}

get_dependent_packages <- function(directory = getwd()) {
  fls <- list.files(
    path = directory,
    pattern = "^.*\\.R$|^.*\\.Rmd$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  pkg_names <- unlist(sapply(fls, parse_packages))
  pkg_names <- unique(pkg_names)
  if (length(pkg_names) == 0) {
    message("warning: no packages found in specified directory")
    return(invisible(NULL))
  }
  return(unname(pkg_names))
}

#' Get R Package Dependencies
#'
#' This function takes a path to a directory and parses the code from all
#' `.R` and `.Rmd` files, retrieving any detected package dependencies, and
#' optionally outputs a `deps.yaml` and `deps.R` file.
#'
#' @param path path to directory
#' @param write_yaml logical - should `deps.yaml` be created?
#' @param write_r logical - should `deps.R` be created?
#' @param include_versions logical - should package versions and github referenced be included?
#'
#' @return silently returns a data.frame with R package details
#' @export
#'
#' @importFrom cli cli_alert_warning cat_bullet
#' @importFrom dplyr bind_rows mutate select
#' @importFrom purrr safely map_depth pluck map flatten_chr compact
#' @importFrom rlang set_names
#' @importFrom yaml write_yaml
get_package_deps <- function(path = getwd(),
                             write_yaml = TRUE,
                             write_r = TRUE,
                             include_versions = TRUE) {
  # get package dependencies based off supplied directory
  # first detect any R scripts or RMD files
  files <- list.files(
    path = path,
    pattern = "^.*\\.R$|^.*\\.Rmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  # loop through files gathering packages using `parse_packages`
  pkg_names_init <- lapply(files, purrr::safely(parse_packages))
  pkg_names <- purrr::map_depth(pkg_names_init, 1, purrr::pluck, "result") %>%
    purrr::map(function(x) {
      if (length(x) == 0) {
        return(NULL)
      } else {
        return(x)
      }
    }) %>%
    purrr::flatten_chr() %>%
    unique()

  if (length(pkg_names) == 0) {
    cli::cli_alert_warning("warning: no packages found in specified directory")
    return(invisible(NULL))
  }

  hold <- lapply(pkg_names, purrr::safely(get_package_details)) %>%
    rlang::set_names(pkg_names)

  out <- purrr::map_depth(hold, 1, purrr::pluck, "result") %>%
    purrr::map(function(x) {
      if (length(x) == 0) {
        return(NULL)
      } else {
        return(x)
      }
    }) %>%
    purrr::compact()

  df <- dplyr::bind_rows(out) %>%
    dplyr::mutate(
      Repository = ifelse(is.na(Repository), "Github", Repository),
      install_cmd = ifelse(
        Repository == "CRAN",
        paste0("remotes::install_version(", shQuote(Package), ", version = ", shQuote(Version), ")"),
        paste0("remotes::install_github(", shQuote(paste0(GithubUsername, "/", Package)), ", ref = ", shQuote(GithubSHA1), ")")
      )
    )

  if (write_yaml) {
    yaml::write_yaml(out, fs::path(path, "deps.yaml"))
    cli::cat_bullet(
      "Created file `deps.yaml`.",
      bullet = "tick",
      bullet_col = "green"
    )
  }

  if (write_r) {
    txt <- paste0(
      "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))\ninstall.packages('remotes')\n",
      paste(df$install_cmd, collapse = "\n")
    )
    cat(txt, file = fs::path(path, "deps.R"))
    cli::cat_bullet(
      "Created file `deps.R`.",
      bullet = "tick",
      bullet_col = "green"
    )
  }

  out_df <- df %>% dplyr::select(package = Package, src = Repository, version = Version, install_cmd)

  return(invisible(out_df))
}


#' @importFrom utils packageDescription
get_package_details <- function(pkg_name) {
  pkg_d <- utils::packageDescription(pkg_name)
  is.cran <- !is.null(pkg_d$Repository) && pkg_d$Repository ==
    "CRAN"
  is.github <- !is.null(pkg_d$GithubRepo)
  is.base <- !is.null(pkg_d$Priority) && pkg_d$Priority ==
    "base"
  if (!is.cran & !is.github & !is.base) {
    stop("CRAN or GitHub info for ", pkg_name, " not found. Other packages repos are not supported.",
      call. = FALSE
    )
  }
  if (is.cran) {
    return(pkg_d[c("Package", "Repository", "Version")])
  }
  if (is.github) {
    return(pkg_d[c(
      "Package", "GithubUsername", "GithubRepo",
      "GithubRef", "GithubSHA1"
    )])
  }
}

#' @importFrom purrr map
parse_packages <- function(fl) {
  lns <- get_lines(fl)
  rgxs <- list(
    library = "(?<=(library\\()|(library\\([\"']{1}))[[:alnum:]|.]+",
    require = "(?<=(require\\()|(require\\([\"']{1}))[[:alnum:]|.]+",
    colon = "[[:alnum:]|.]*(?=:{2,3})"
  )
  found_pkgs <- purrr::map(rgxs, finder, lns = lns) %>%
    unlist() %>%
    unique()
  found_pkgs <- found_pkgs[!found_pkgs %in% c("", " ")]
  return(found_pkgs)
}

# from automagic 0.5.1
#' @importFrom formatR tidy_source
#' @importFrom knitr purl
get_lines <- function(file_name) {
  if (grepl(".Rmd", file_name, fixed = TRUE)) {
    tmp.file <- tempfile()
    knitr::purl(input = file_name, output = tmp.file, quiet = TRUE)
    file_name <- tmp.file
  }
  lns <- tryCatch(
    formatR::tidy_source(file_name,
      comment = FALSE,
      blank = FALSE, arrow = TRUE, brace.newline = TRUE, output = FALSE
    )$text.mask,
    error = function(e) {
      message(paste("Could not parse R code in:", file_name))
      message("   Make sure you are specifying the right file name")
      message("   and check for syntax errors")
      stop("", call. = FALSE)
    }
  )
  if (is.null(lns)) {
    stop("No parsed text available", call. = FALSE)
  }
  return(lns)
}

# from automagic 0.5.1
finder <- function(rgx, lns) {
  regmatches(lns, gregexpr(rgx, lns, perl = TRUE)) %>% unlist()
}



# dir <- "inst/examples/test_shiny_app"
# get_pkgdeps(dir)

get_cran_deps <- function(deps) {
  deps[sapply(deps, function(el) identical(el$Repository, "CRAN"))]
}

get_gh_deps <- function(deps) {
  deps[sapply(deps, function(el) !is.null(el$GithubRepo))]
}

cran_packages_cmd <- function(cran_deps) {
  cran_deps_string <- unlist(lapply(cran_deps, cran_install_string))
  paste0(
    "# CRAN R packages \n",
    paste(cran_deps_string, collapse = " \n"),
    "\n"
  )
}

gh_packages_cmd <- function(gh_deps) {
  github_deps_string <- unlist(lapply(gh_deps, github_install_string))
  paste0(
    "# GitHub R packages \n",
    paste(github_deps_string, collapse = " \n"),
    "\n"
  )
}


cran_install_string <- function(dep) {
  paste0("RUN R -e \"remotes::install_version('", dep$Package, "', version = '", dep$Version, "', upgrade = 'never')\"")
}

github_install_string <- function(dep) {
  paste0("RUN R -e \"remotes::install_github('", dep$GithubUsername, "/", dep$Package, "', ref = '", dep$GithubSHA1, "', upgrade='never')\"")
}

r_command_string <- function(command) {
  paste0("RUN R -e \"", command, "\"")
}
