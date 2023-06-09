# No Remotes ----
# Attachments ----
to_install <- c("cli", "crayon", "formatR", "fs", "glue", "here", "jsonlite", "knitr", "magrittr", "pak", "purrr", "remotes", "rlang", "stringr", "tibble", "usethis", "whoami", "xfun", "yaml")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i, quietly = TRUE)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

