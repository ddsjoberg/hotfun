# TODO: Set seed for checks?
set.seed(6038503)

# TODO: Checks
# Check it works with different confidence levels

# Check it runs with no error with or without covariates
binarydiff(data = trial, outcome = "response", predictor = "trt")

binarydiff(data = trial, outcome = "response", predictor = "trt",
           covariates = c("age", "stage"), bootstrapn = 200)

# Check error if variables don't exist
binarydiff(trial, outcome = "response", predictor = "trt_new")
binarydiff(trial, outcome = "response_new", predictor = "trt")

# Check it captures predictor/outcome with more than 2 non-missing levels
binarydiff(data = trial %>% dplyr::mutate(response2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
           outcome = "response2", predictor = "trt")

binarydiff(data = trial %>% dplyr::mutate(trt2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
           outcome = "response", predictor = "trt2")

# Check it works with any type predictor/outcome (character, numeric, factor)
# Character
binarydiff(data = trial %>% dplyr::mutate(response = as.character(response)),
           outcome = "response", predictor = "trt")

# Numeric
binarydiff(data = trial %>% dplyr::mutate(trt = as.numeric(as.factor(trt))),
           outcome = "response", predictor = "trt")

# Factor
binarydiff(data = trial %>% dplyr::mutate(trt = as.factor(trt), response = as.factor(response)),
           outcome = "response", predictor = "trt")

# Check REV option works, with and without covariates
binarydiff(data = trial, outcome = "response", predictor = "trt", rev = TRUE)
binarydiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
           rev = TRUE, bootstrapn = 200)

# Check it works if no column labels
binarydiff(data = trial %>% dplyr::mutate(trt = as.numeric(as.factor(trt))),
           outcome = "response", predictor = "trt")

# Check it works if no label for variable
binarydiff(data = trial %>% dplyr::mutate(response2 = sample(c(0, 1), size = nrow(trial), replace = TRUE)),
           outcome = "response2", predictor = "trt")

# Confirm it works with different confidence levels
binarydiff(data = trial, outcome = "response", predictor = "trt", conflevel = 0.90)
binarydiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
           conflevel = 0.90, bootstrapn = 200)
