require(usethis)
require(actions)
require(gh)
require(pkgdown)
require(rmarkdown)

gh::gh_whoami()
gh::gh_token()

usethis::use_github_action("pkgdown")
usethis::use_github_action("check-standard")
usethis::use_github_action("check-release")
usethis::use_github_action("document")
usethis::use_github_action("lint")
usethis::use_github_action("style")

usethis::use_github_actions_badge(name = "check.yml")

workflows <- fs::dir_ls(".github/workflows") |>
    basename() |>
    as.character()

workflows |> purrr::walk(
    usethis::use_github_actions_badge
)

devtools::build_readme()

library(templateeR)

usethis::use_github_labels(
    labels = gh_labels$label, colours = gh_labels$colour,
    descriptions = gh_labels$description, delete_default = TRUE
)

usethis::use_pkgdown_github_pages()
