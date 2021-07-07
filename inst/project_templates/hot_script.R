# if repo is hosted at https://github.mskcc.org/Analytic-Projects do the following:
# 1. Set repo to private
# 2. Add phi topic hashtag

# get url of origin
remote_origin_url <-
  gert::git_remote_list(repo = path) %>%
  dplyr::filter(.data$name %in% "origin") %>%
  dplyr::pull(.data$url)


# if repo is in Analytic-Projects org, then add attributes
if (isTRUE(fs::path_dir(remote_origin_url) == "https:/github.mskcc.org/Analytic-Projects")) {
  # save repo name
  repo_name <-
    remote_origin_url %>%
    fs::path_file() %>%
    fs::path_ext_remove()

  # get MSK GitHub PAT
  msk_pat <- mskRutils::github_msk_token()

  if (msk_pat != "") {
    # set repo to private
    gh::gh(
      "PATCH /repos/:org/:repo",
      org = "Analytic-Projects",
      repo = repo_name,
      private = TRUE,
      .api_url = "https://github.mskcc.org/api/v3",
      .token = msk_pat
    )
    usethis::ui_done("Setting GitHub repo visability to {usethis::ui_field('private')}")

    # add topic phi hashtag
    gh::gh(
      "PUT /repos/:org/:repo/topics",
      org = "Analytic-Projects",
      repo = repo_name,
      names = array("phi"),
      .api_url = "https://github.mskcc.org/api/v3",
      .accept = "application/vnd.github.mercy-preview+json",
      .token = msk_pat
    )
    usethis::ui_done("Adding GitHub topic {usethis::ui_field('#phi')}")

  }
  else {
    paste(
      "Cannot finish configuring GitHub repo, because there is no MSK GitHub",
      "PAT available. Ensure the repo is set to {usethis::ui_field('private')}",
      "and {usethis::ui_field('#phi')} is added as a topic."
    ) %>%
    usethis::ui_oops()

    usethis::ui_todo("Setup an  MSK GitHub PAT with {usethis::ui_code('?mskRutils::github_msk_token()')}")
  }
}
