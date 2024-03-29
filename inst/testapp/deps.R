options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))
install.packages('remotes')
remotes::install_version("shiny", version = "1.7.1")
remotes::install_version("shinyWidgets", version = "0.7.0")
remotes::install_github("thinkR-open/attachment", ref = "e064f7ddee4e2d103719e2be29a6a78135fff5b4")
remotes::install_version("config", version = "0.3.1")
remotes::install_version("magick", version = "2.7.3")
remotes::install_version("tinytex", version = "0.39")