library(shiny)
library(shinyWidgets)
library(attachment) # add github only package
library(config) # library(tidyverse)

# library a package with known sysreqs
library(magick)
library(tinytex)

# adding a test for an invalid package
# library(dplyr)
"123::456"
"pkg.name::fname"

app_config <- config::get()
