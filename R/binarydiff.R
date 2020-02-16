library(tidyverse)

auto <- haven::read_dta("C:/Program Files (x86)/Stata15/ado/base/a/auto.dta")
data <-
  auto %>% filter(rep78 %in% c(3, 4)) %>%
  mutate(rep78 = rep78 - 3) %>%
  mutate(headroomcat =
           factor(case_when(
             headroom <= 2 ~ 1,
             headroom > 2 & headroom <= 3.5 ~ 2,
             headroom > 3.5 ~ 3
           )),
         mpgcat =
           factor(case_when(
             mpg <= 15 ~ 1,
             mpg > 15 & mpg <= 20 ~ 2,
             mpg > 20 & mpg <= 25 ~ 3,
             mpg > 25 ~ 4
           ))
  )


outcome <- "rep78"
predictor <- "foreign"
covariates <- c("price", "weight", "headroomcat", "mpgcat")
#covariates <- NULL
conflevel <- 0.95
bootstrapn <- 10

#binarydiff <- function(data, outcome, predictor, covariates = NULL, rev = FALSE, conflevel = 0.95, bootstrapn = 2000) {

  ### SETUP-------------------

# TODO: How to coerce factor to 0/1 always?
  # Rename outcome variable and keep only complete cases
  data <- data %>%
    dplyr::mutate(predictor_name = as.numeric(predictor),
                  outcome_name = outcome) %>%
    dplyr::select(outcome_name, predictor_name, tidyselect::all_of(covariates)) %>%
    dplyr::filter(stats::complete.cases(.) == TRUE)

  # TODO: If "rev" option is specified - this involves flipping levels... figure this out

  ### UNADJUSTED DIFFERENCE------------------

  uvresults <-
  dplyr::tibble(outcome = outcome) %>%
  dplyr::mutate(N = nrow(data)) %>%
  dplyr::bind_cols(
    uv_binarydiff(data, conflevel = conflevel)
  )

  # If no covariates specified, export unadjusted difference
  if(is.null(covariates)) {

    # TODO: Figure out table formatting here rather than exporting just data...
    return(uvresults)

  }

### ADJUSTED DIFFERENCE-----------------

  # Save out formula for model
  model_formula <-
   glue::glue("outcome_name ~ predictor_name + ",
             glue::glue_collapse(covariates, sep = " + ")) %>%
    as.character()

  # Create logistic regression model
  model_obj <-
    stats::glm(
      as.formula(model_formula),
      data = data,
      family = "binomial"
    )

  # New dataset with mean/mode of covariates
  df_newdata <- create_newdata(data, covariates)

  # Calculate adjusted probability and then difference between groups
  df_prediction <-
    broom::augment(
      model_obj,
      newdata = df_newdata,
      type.predict = "response"
    ) %>%
    # Reshape
    dplyr::select(predictor_name, tidyselect::all_of(covariates), pred = .fitted) %>%
    tidyr::pivot_wider(
      names_from = predictor_name,
      names_prefix = "pred",
      values_from = "pred"
    ) %>%
    dplyr::mutate(estimate = (pred1 - pred0))

  # pvalue from adjusted model
  pval_predictor <-
    broom::tidy(model_obj) %>%
    dplyr::filter(term == "predictor_name") %>%
    dplyr::pull(p.value)

  # Create list of bootstrap indicators so we don't have to save out data 2000x
  bs_list <- purrr::map(seq_along(1:bootstrapn), ~ sample.int(nrow(data), replace = TRUE))

  # Create map dataframe for all datasets
  df_bs_map <-
    dplyr::tibble(idn = 1:bootstrapn) %>%
    dplyr::mutate(bs_assignment = purrr::map(idn, ~ bs_list[[..1]]))

  # Bootstrapping
  df_bs_results <-
    df_bs_map %>%
    mutate(
      # Create models
      model_obj_bs =
        pmap(
          list(model_formula, bs_assignment),
          ~ glm(
            as.formula(..1),
            data = data %>% slice(..2),
            family = "binomial"
          )
        ),
      df_newdata_bs =
        pmap(
          # Save out new data for predictions
          list(bs_assignment),
          ~ create_newdata(data %>% slice(..1), covariates)
        ),
      # Predicted probabilities and differences
      df_prediction_bs =
        pmap(
          list(model_obj_bs, df_newdata_bs),
          ~ broom::augment(
            ..1,
            newdata = ..2,
            type.predict = "response"
          ) %>%
            # Reshape
            select(predictor_name, tidyselect::all_of(covariates), pred = .fitted) %>%
            pivot_wider(
              names_from = predictor_name,
              names_prefix = "pred",
              values_from = "pred"
            ) %>%
            mutate(estimate = (pred1 - pred0))
        )
    )

  # Collapse predicted differences across all bootstraps
  df_ci_results <-
    df_bs_results %>%
    dplyr::select(df_prediction_bs) %>%
    tidyr::unnest(df_prediction_bs) %>%
    dplyr::summarize(
      conf.low = quantile(estimate, 0.025, na.rm = TRUE),
      conf.high = quantile(estimate, 0.975, na.rm = TRUE)
    )

  # Merge in with main results and format
  df_final_results <-
    dplyr::bind_cols(
      uvresults %>% dplyr::select(-c(estimate, conf.low, conf.high, p.value)),
      df_prediction %>% dplyr::select(estimate)
    ) %>%
    bind_cols(df_ci_results) %>%
    mutate(
      variable = outcome,
      conf.low = conf.low,
      conf.high = conf.high,
      estimate = estimate,
      p.value = pval_predictor
    ) %>%
    select(variable, N, pred0, pred1, estimate, conf.low, conf.high, p.value)

  # TODO: RETURN THIS AS FORMATTED TABLE (LOOK AT TBL_ANCOVA CODE)

  return(df_final_results)

#}
