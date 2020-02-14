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

#binarydiff <- function(data, outcome, predictor, covariates = NULL, rev = FALSE, conflevel = 0.95, bootstrapn = 2000) {

  ### SETUP-------------------

# TODO: How to coerce factor to 0/1 always?
  # Rename outcome variable and keep only complete cases
  data <- data %>%
    dplyr::rename(outcome_name = outcome, predictor_name = predictor) %>%
    dplyr::mutate(predictor_name = as.numeric(predictor_name)) %>%
    dplyr::select(outcome_name, predictor_name, tidyselect::all_of(covariates)) %>%
    dplyr::filter(stats::complete.cases(.) == TRUE)

  # TODO: If "rev" option is specified - this involves flipping levels... figure this out

  ### UNADJUSTED DIFFERENCE------------------

  uvresults <-
  dplyr::tibble(outcome = outcome) %>%
  dplyr::mutate(N = nrow(data)) %>%
  dplyr::bind_cols(
    dplyr::tibble(outcome = outcome),
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

  # N and pvalue from adjusted model
  # TODO: change this to broom::Tidy - how to get N?
  N_pval_predictor <-
    model_obj %>%
    gtsummary::tbl_regression() %>%
    pluck("table_body") %>%
    filter(variable == "predictor_name" & !is.na(p.value)) %>%
    select(N, p.value)

  # Create list of bootstrap indicators so we don't have to save out data 2000x
  bs_list <- map(seq_along(1:bootstrapn), ~ sample.int(nrow(data), replace = TRUE))

  # Create map dataframe for all 2000 datasets
  df_bs_map <-
    tibble(idn = 1:bootstrapn) %>%
    mutate(bs_assignment = map(idn, ~ bs_list[[..1]]))

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
            family = family
          )
        ),
      df_newdata_bs =
        pmap(
          # Save out new data for predictions
          list(bs_assignment),
          ~ bind_cols(
            # Mean for continuous
            data %>%
              slice(..1) %>%
              select(covariates) %>%
              summarize_if(is.numeric, mean, na.rm = TRUE) %>%
              unique(),
            # Mode for factors
            data %>%
              slice(..1) %>%
              select(covariates) %>%
              summarize_if(is.factor, Mode) %>%
              unique()
          ) %>%
            mutate(freq = 2) %>%
            uncount(freq) %>%
            bind_cols(
              data %>% select(predictor_name) %>% unique()
            )
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
            select(predictor_name, covariates, pred = .fitted) %>%
            pivot_wider(
              names_from = predictor_name,
              names_prefix = "pred",
              values_from = "pred"
            ) %>%
            mutate(estimate = (pred2 - pred1)*revnum)
        )
    )

  # Collapse predicted differences across all bootstraps
  df_ci_results <-
    df_bs_results %>%
    select(df_prediction_bs) %>%
    unnest(df_prediction_bs) %>%
    summarize(
      conf.low = quantile(estimate, 0.025, na.rm = TRUE),
      conf.high = quantile(estimate, 0.975, na.rm = TRUE)
    )

  # Merge in with main results and format
  if(family == "binomial") conversion <- 100 else conversion <- 1
  df_final_results <-
    bind_cols(
      df_unadjusted_pred,
      df_prediction %>% select(estimate)
    ) %>%
    bind_cols(df_ci_results) %>%
    bind_cols(N_pval_predictor) %>%
    mutate(
      variable = outcome,
      stat_1 = style_sigfig(stat_1*conversion),
      stat_2 = style_sigfig(stat_2*conversion),
      conf.low = conf.low*conversion,
      conf.high = conf.high*conversion,
      estimate = estimate*conversion
    ) %>%
    select(variable, N, stat_1, stat_2, estimate, conf.low, conf.high, p.value)

  return(df_final_results)

}
