## code to prepare project template goes here

# hot template
project_template <-
  quote(list(
  readme = list(
    template_filename = fs::path_package("project_templates/hot_readme.md", package = 'hotfun'),
    filename = "README.md",
    copy = FALSE
  ),
  gitignore = list(
    template_filename = fs::path_package("project_templates/hot_gitignore.txt", package = 'hotfun'),
    filename = ".gitignore",
    copy = TRUE
  ),
  data_date = list(
    template_filename = fs::path_package("project_templates/hot_data_date.txt", package = 'hotfun'),
    filename = "data_date.txt",
    copy = FALSE
  ),
  setup = list(
    template_filename = fs::path_package("project_templates/hot_setup.Rmd", package = 'hotfun'),
    filename = glue::glue("scripts/setup1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
    copy = FALSE
  ),
  analysis = list(
    template_filename = fs::path_package("project_templates/hot_analysis.Rmd", package = 'hotfun'),
    filename = glue::glue("scripts/analysis1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
    copy = FALSE
  ),
  report = list(
    template_filename = fs::path_package("project_templates/hot_report.Rmd", package = 'hotfun'),
    filename = glue::glue("scripts/report1_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.Rmd"),
    copy = FALSE
  ),
  sap = list(
    template_filename = fs::path_package("project_templates/hot_sap.docx", package = 'hotfun'),
    filename = glue::glue("SAP - {folder_name}.docx"),
    copy = TRUE
  ),
  doc_template = list(
    template_filename = fs::path_package("project_templates/doc_template.docx", package = 'hotfun'),
    filename = "templates/doc_template.docx",
    copy = TRUE
  ),
  derived_vars = list(
    template_filename = fs::path_package("project_templates/hot_derived_variables.xlsx", package = 'hotfun'),
    filename = glue::glue("scripts/derived_variables_{stringr::str_split(folder_name, pattern = ' |-', simplify = T)[, 1] %>% tolower()}.xlsx"),
    copy = TRUE
  ),
  rproj = list(
    template_filename = fs::path_package("project_templates/default_rproj.Rproj", package = 'hotfun'),
    filename = glue::glue("_rstudio_project.Rproj"),
    copy = TRUE
  ),
  bib = list(
    template_filename = fs::path_package("project_templates/hot_references.bib", package = 'hotfun'),
    filename = "templates/references.bib",
    copy = TRUE
  ),
  csl = list(
    template_filename = fs::path_package("project_templates/european-urology.csl", package = 'hotfun'),
    filename = "templates/european-urology.csl",
    copy = TRUE
  ),
  # only add Rprofile if renv was used
  rprofile =
    switch(
      renv,
      list(
        template_filename =
          fs::path_package(package = "starter", "project_templates/default_rprofile.R"),
        filename = stringr::str_glue(".Rprofile"),
        glue = TRUE
      )
    )
))
attr(project_template, "label") <- "H.O.T. Project Template"
usethis::use_data(project_template, overwrite = TRUE)
