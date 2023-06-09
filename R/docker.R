#' Create Dockerfile from
#'
#' @param path defaults to getwd()
#' @param base_image base image for `FROM` in Dockerfile
#' @param app_config app config, defaults to `default`
#' @param maintainer maintainer - defaults to `fullname` from [whoami::whoami()]
#' @param date date - defaults to [base::Sys.Date()]
#' @param packages R package dependencies returned from [get_package_deps()]
#' @param sysreqs logical - should sysreqs be included in image (via [get_sysreqs()])
#' @param additional_r_commands any additional r commands to include in image
#'
#' @return invisibly returns list of package and system dependencies
#' @export
#'
#' @importFrom usethis use_template
create_dockerfile <- function(path = getwd(),
                              base_image = "rocker/r-ver:latest",
                              app_config = "default",
                              mainainer = whoami::whoami()["fullname"],
                              date = Sys.Date(),
                              additional_r_commands = NULL,
                              port = "8080") {
  pkgdeps <- get_package_deps(path)
  packages <- pkgdeps$package

  sysreqs <- get_sysreqs(packages)

  system_deps_string <- paste(paste0("  ", sysreqs), collapse = " \\ \n")

  sysreqs_out <- paste0(
    "RUN apt-get update && apt-get install -y \\ \n",
    system_deps_string
  )

  usethis::use_template(
    "Dockerfile",
    save_as = as.character(fs::path_rel(path = fs::path(path, "Dockerfile"))),
    data = list(
      base_image = base_image,
      app_config = app_config,
      maintianer = maintainer,
      # date = as.character(date),
      sysreqs = sysreqs_out,
      additional_r_commands = additional_r_commands,
      port = port
    ),
    package = "buildtools"
  )

  out <- list(
    package_deps = pkgdeps,
    sysreqs = sysreqs
  )

  return(invisible(out))
}

#' Create .dockerignore from template
#'
#' @param open logical - open file?
#'
#' @export
#'
#' @importFrom usethis use_template
create_dockerignore <- function(open = TRUE) {
  usethis::use_template(
    "dockerignore",
    save_as = ".dockerignore",
    ignore = FALSE,
    open = open,
    package = "buildtools"
  )
}

#' Find the Docker Executable
#'
#' @return character string that is location of the docker executable
#' @export
find_docker_cmd <- function() {
  docker_cmd <- Sys.which("docker")
  if (length(docker_cmd) == 0) {
    stop(paste(
      "\n", "Docker command not found. ", "\n",
      "Please install docker: ", "\n",
      "https://www.docker.com/products/overview#/install_the_platform"
    ))
  }
  docker_cmd
}

# start_docker_service <- function() {
#   if (check_docker_running()) stop("Docker service is already running!")
# }

#' Check that the Docker Daemon/Service is Running
#'
#' @return Logical TRUE/FALSE
#' @export
check_docker_running <- function() {
  check_cmd <- system2("powershell", "-noprofile (get-service -name 'com.docker.service').Status", stdout = TRUE)
  return(check_cmd == "Running")
}

#' Build Docker Shiny Application
#'
#' @param app_name Name of application to make (defaults to working directory)
#'
#' @export
build_docker_app <- function(app.name = basename(getwd())) {
  docker_cmd <- find_docker_cmd()
  system2(docker_cmd, c("build", "-t", app.name, "."))
}

#' Use Docker
#'
#' Add or modify the Dockerfile in the current project.
#'
#' @param rver Which r version to use, defaults to current version.
#' @param stack Which stack to use, possible values are `c("r-ver", "rstudio", "tidyverse", "verse", "geospatial")`.
#' @param date Which date should be used for package instalation, defaults to today.
#' @param file Which file to save to
#' @param open Open the newly created file for editing? Happens in RStudio, if applicable, or via utils::file.edit() otherwise.
#' @name docker
#' @export

#' @importFrom glue glue
#' @importFrom usethis use_template
use_docker <- function(dir = getwd(),
                       rver = NULL,
                       stack = "verse",
                       date = Sys.Date(),
                       file = "Dockerfile",
                       open = TRUE) {
  deps_path <- fs::path(dir, "deps.yaml")

  if (!file.exists(deps_path)) {
    usethis::ui_warn("No deps.yaml file found. Creating deps.yaml..")
    create_deps_yaml(dir)
  }

  deps <- yaml::read_yaml(file = deps_path)

  if (is.null(rver)) {
    rver <- glue::glue(R.version$major, ".", R.version$minor)
  }
  usethis::use_template(
    "Dockerfile",
    file,
    data = list(
      rver = rver,
      stack = stack,
      date = date
    ),
    ignore = FALSE,
    open = open,
    package = "jimstemplates"
  )
}

create_deps_yaml <- function(directory = getwd()) {
  pkg_names <- get_dependent_packages(directory) %>% unique()
  lapply(pkg_names, get_package_details) %>%
    yaml::as.yaml() %>%
    cat(file = file.path(directory, "deps.yaml"))
}

get_dependent_packages <- function(directory = getwd()) {
  fls <- list.files(
    path = directory, pattern = "^.*\\.R$|^.*\\.Rmd$",
    full.names = TRUE, recursive = TRUE
  )
  pkg_names <- unlist(sapply(fls, parse_packages))
  pkg_names <- unique(pkg_names)
  if (length(pkg_names) == 0) {
    message("warning: no packages found in specified directory")
    return(invisible(NULL))
  }
  return(unname(pkg_names))
}

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
      message(paste(
        "Could not parse R code in:",
        file_name
      ))
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

finder <- function(rgx, lns) {
  regmatches(lns, gregexpr(rgx, lns, perl = TRUE)) %>% unlist()
}

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

get_package_details <- function(pkg_name) {
  pkg_d <- packageDescription(pkg_name)
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
      "Package", "GithubUsername",
      "GithubRepo", "GithubRef", "GithubSHA1"
    )])
  }
}

#' Add dependencies to Dockerfile
#'
#' Adds package dependencies as a new RUN statement to Dockerfile.
#' Sorts packages first into source (cran & github) and then alphabetically.
#'
#' @param packages Which packages to add.
#' @param github Are there github packages?
#' @param strict Defaults to TRUE, force a specific version for github packages.
#' @param file Where is the 'Dockerfile'?
#' @param write Should the 'Dockerfile' be modified?
#' @param open Should the file be opened?
#' @param append Should the return value be appended to the 'Dockerfile'?
#' @export
#' @importFrom stringr str_detect
#' @importFrom usethis ui_warn ui_code ui_stop
use_docker_packages <- function(packages,
                                github = NULL,
                                strict = TRUE,
                                file = "Dockerfile",
                                write = TRUE,
                                open = write,
                                append = TRUE) {
  # github stuff has these symbols
  on_github <- packages[stringr::str_detect(packages, "[/|@]")]
  # everything else is assumed to be on cran
  on_cran <- packages[!(packages %in% on_github)]
  if (!isTRUE(github) & (length(on_github) > 0)) {
    usethis::ui_warn("Some packages seem to come from GitHub.
            Set {usethis::ui_code('github = TRUE')} to silence this warning.")
  }
  if (isTRUE(strict) & any(!stringr::str_detect(on_github, "@"))) {
    usethis::ui_stop("Some github packages are without fixed version. Use the following scheme:
            {usethis::ui_code('author/package@version')}
            version can be a git tag or hash or
            set {usethis::ui_code('strict = FALSE')} on your own risk.")
  }

  # sort alphabetically and remove duplicates
  on_github <- unique(on_github)
  on_github <- sort(on_github)
  on_cran <- unique(on_cran)
  on_cran <- sort(on_cran)

  # construct Dockerfile entries
  # and write them appended to Dockerfile
  to_write <- character()

  if (length(on_cran) > 0) {
    cran_entry <- docker_entry_install(
      on_cran,
      "install2.r",
      "--error --skipinstalled"
    )
    to_write <- c(to_write, cran_entry)
  }
  if (length(on_github) > 0) {
    github_entry <- docker_entry_install(on_github, "installGithub.r")
    to_write <- c(to_write, github_entry)
  }
  docker_entry(to_write, file, write, open, append, quiet = TRUE)
}

#' @importFrom stringr str_c
use_docker_apt <- function(apt, update = TRUE, file = "Dockerfile", write = TRUE, open = write, append = TRUE) {
  if (length(apt) == 0L) {
    return(NULL)
  }
  to_write <- "RUN "
  if (update) to_write <- stringr::str_c(to_write, "apt-get update -y && ")
  to_write <- stringr::str_c(to_write, "apt-get install -y ")
  to_write <- stringr::str_c(to_write, stringr::str_c(apt, collapse = " "))
  docker_entry(to_write, file, write, open, append, quiet = TRUE)
}

#' @importFrom fs file_exists
#' @importFrom glue glue
#' @importFrom usethis ui_oops ui_path ui_todo ui_code proj_path ui_done ui_value edit_file
#' @importFrom xfun read_utf8 write_utf8
docker_entry <- function(entry, file = "Dockerfile", write, open, append, quiet = FALSE) {
  if (!fs::file_exists(file)) {
    usethis::ui_oops(glue::glue("There is no {usethis::ui_path(file)}!"))
    usethis::ui_todo(
      glue::glue(
        "Run {usethis::ui_code('use_docker()')} to create {usethis::ui_path(file)}."
      )
    )
    return(invisible(NULL))
  }
  # read dockerfile
  path <- usethis::proj_path(file)
  dockerfile <- xfun::read_utf8(path)
  if (!quiet) {
    usethis::ui_done("Adding {usethis::ui_value(entry)} to {usethis::ui_path(file)}")
  }
  if (append) entry <- c(dockerfile, entry)
  if (write) {
    xfun::write_utf8(entry, path)
    if (open) {
      usethis::edit_file(path)
    }
    return(invisible(entry))
  } else {
    return(entry)
  }
}

#' @importFrom stringr str_c
docker_entry_install <- function(packages, cmd, flags = NULL) {
  entry <- stringr::str_c("RUN", cmd, flags, "\\\ ", sep = " ")
  if (length(packages) == 1L) {
    entry <- c(entry, stringr::str_c("  ", packages))
  } else {
    entry <- c(
      entry,
      stringr::str_c("  ", packages[-length(packages)], " \\\ "),
      stringr::str_c("  ", packages[length(packages)])
    )
  }
  entry
}

#' @importFrom stringr str_detect
docker_get_install <- function(dockerfile) {
  starts <- stringr::str_detect(dockerfile, "^(RUN install)(.*)(\\.)[Rr](.*)$")
  possible_range <- c(rep(which(starts), each = 2L)[-1], length(dockerfile))
  possible_list <- apply(matrix(possible_range, ncol = 2L), 1, function(x) list(x))
  possible_pos <- lapply(possible_list, function(x) seq(x[[1]][1], x[[1]][2]))
  possible <- lapply(possible_pos, function(x) dockerfile[x])
  pos_raw <- lapply(possible, function(x) which(stringr::str_detect(x, "^(  )")))
  out <- vector("list", length(pos_raw))
  for (i in seq_along(pos_raw)) {
    out[[i]] <- c(possible[[i]][1], possible[[i]][pos_raw[[i]]])
  }
  return(out)
}

#' @importFrom fs file_exists
#' @importFrom glue glue
#' @importFrom stringr str_detect str_extract
#' @importFrom usethis ui_oops ui_path ui_todo ui_code
docker_get_packages <- function(file = "Dockerfile") {
  if (!fs::file_exists(file)) {
    usethis::ui_oops(glue::glue("There is no {usethis::ui_path(file)}!"))
    usethis::ui_todo(
      glue::glue(
        "Run {usethis::ui_code('use_docker()')} to create {usethis::ui_path(file)}."
      )
    )
    return(invisible(NULL))
  } else {
    dockerfile <- readLines(file)
  }
  entry <- docker_get_install(dockerfile)
  packages_raw <- lapply(entry, function(x) x[stringr::str_detect(x, "^(  )")])
  packages <- lapply(packages_raw, function(x) stringr::str_extract(x, "[a-zA-z]+"))
  packages_sorted <- sort(unique(unlist(packages)))
  return(packages_sorted)
}

#' @importFrom stringr str_extract_all str_c str_to_lower str_remove
dir2imagename <- function(dir) {
  dir <- basename(dir)
  stopifnot(length(dir) == 1L)
  dir <- stringr::str_extract_all(dir, "[A-z0-9]")[[1]]
  dir <- stringr::str_c(dir, collapse = "")
  dir <- stringr::str_to_lower(dir)
  dir <- stringr::str_remove(dir, "^[0-9]")
  dir
}
