# 03_prediction_models.R =============================================================================

#
# Prediction of reported COVID-19 cases from wastewater SARS-CoV-2 levels
# using rolling Poisson regression models.
#
# Analyses are performed for:
# - 5- and 6-week training windows
# - Wastewater lag 1 and lag 2 weeks
# - Regional WWTPs
# - Stockholm indicator
# - National indicator
#
# For each model, COVID-19 cases in the following week are predicted from
# lagged wastewater SARS-CoV-2 levels.
#
# Prediction performance is evaluated using:
# - Mean squared prediction error (MSPE)
# - Proportion of observed values within the 95% prediction interval
#
# Prediction period:
# 2023-W30 to 2024-W03
#
# Outputs:
# - prediction_results.csv
# - prediction_model_summary.csv

# Packages ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(here)
library(ciTools)


# Import data =============================================================================


data_file <- here(
  "data",
  "Supporting_data_s1.xlsx"
)

national <- read_excel(
  data_file,
  sheet = "National level"
)

stockholm <- read_excel(
  data_file,
  sheet = "Stockholm level"
)

regional <- read_excel(
  data_file,
  sheet = "Regional level"
)


# Prepare data =============================================================================

prepare_prediction_data <- function(data, location_name = NULL) {

  data <- data %>%
    mutate(
      ww_level = as.numeric(ww_level),
      year = as.integer(sub("-.*", "", year_week)),
      week = as.integer(sub(".*-", "", year_week))
    ) %>%
    filter(
      (year == 2023 & week >= 30) |
        (year == 2024 & week <= 3)
    ) %>%
    arrange(year, week) %>%
    mutate(
      week_lop = row_number() + 186
    )

  if (!is.null(location_name)) {
    data <- data %>%
      mutate(
        plant = location_name
      )
  }

  data
}

## Regional WWTPs ----------------------------------------------------------

regional_prediction <- regional %>%
  split(.$plant) %>%
  purrr::map_dfr(
    function(x) {
      
      prepare_prediction_data(x) %>%
        mutate(
          plant = unique(x$plant)
        )
    }
  )


## Stockholm ---------------------------------------------------------------

stockholm_prediction <- prepare_prediction_data(
  stockholm,
  location_name = "Stockholm"
)

national_prediction <- prepare_prediction_data(
  national,
  location_name = "National"
)


## National ----------------------------------------------------------------

national_prediction <- prepare_prediction_data(
  national,
  location_name = "National"
)



## Combine -----------------------------------------------------------------

master_data <- bind_rows(
  regional_prediction,
  stockholm_prediction,
  national_prediction
) %>%
  arrange(
    plant,
    year,
    week
  )


# Prediction model function =============================================================================

run_prediction_model <- function(
    data,
    training_weeks,
    lag_weeks
) {
  
  data <- data %>%
    arrange(
      year,
      week
    ) %>%
    mutate(
      ww_lagged = dplyr::lag(
        ww_level,
        n = lag_weeks
      )
    )
  
  prediction_rows <- list()
  result_number <- 1
  
  
  # Each model uses the preceding training window and predicts
  # the immediately following week.
  for (
    start_row in seq_len(
      nrow(data) - (training_weeks + 1)
    )
  ) {
    
    training_rows <- start_row:(
      start_row + training_weeks - 1
    )
    
    prediction_row <- start_row + training_weeks
    
    
    # Training data
    training_data <- data[
      training_rows,
    ] %>%
      filter(
        !is.na(case),
        !is.na(ww_lagged)
      )
    
    
    # Prediction week
    new_data <- data[
      prediction_row,
    ]
    
    
    # Skip if wastewater value required for prediction is missing
    if (
      nrow(training_data) < 2 ||
      is.na(new_data$ww_lagged)
    ) {
      next
    }
    
    
    # Poisson regression
    model <- tryCatch(
      glm(
        case ~ ww_lagged,
        family = poisson(
          link = "log"
        ),
        data = training_data
      ),
      error = function(e) NULL
    )
    
    
    if (is.null(model)) {
      next
    }
    
    
    # Reproducible simulation-based 95% prediction interval
    set.seed(9000)
    
    prediction <- tryCatch(
      new_data %>%
        add_pi(
          model,
          names = c(
            "lower_pi",
            "upper_pi"
          ),
          alpha = 0.05,
          nSims = 20000
        ),
      error = function(e) NULL
    )
    
    
    if (is.null(prediction)) {
      next
    }
    
    
    prediction_rows[[result_number]] <- tibble(
      plant = new_data$plant,
      year_week = new_data$year_week,
      year = new_data$year,
      week = new_data$week,
      week_lop = new_data$week_lop,
      
      training_weeks = training_weeks,
      lag = lag_weeks,
      
      case = new_data$case,
      ww_level = new_data$ww_level,
      ww_lagged = new_data$ww_lagged,
      
      predicted_cases = prediction$pred,
      lower_pi = prediction$lower_pi,
      upper_pi = prediction$upper_pi
    )
    
    result_number <- result_number + 1
  }
  
  
  if (length(prediction_rows) == 0) {
    return(
      tibble()
    )
  }
  
  
  bind_rows(
    prediction_rows
  ) %>%
    
    mutate(
      squared_prediction_error = (
        predicted_cases - case
      )^2,
      
      within_95_pi = (
        case >= lower_pi &
          case <= upper_pi
      )
    )
}


# Run all prediction models =============================================================================

plants <- unique(
  master_data$plant
)

training_windows <- c(
  5,
  6
)

lags <- c(
  1,
  2
)

all_predictions <- list()

result_number <- 1


for (plant_name in plants) {
  
  plant_data <- master_data %>%
    filter(
      plant == plant_name
    ) %>%
    arrange(
      year,
      week
    )
  
  
  for (training_weeks in training_windows) {
    
    for (lag_weeks in lags) {
      
      model_results <- run_prediction_model(
        data = plant_data,
        training_weeks = training_weeks,
        lag_weeks = lag_weeks
      )
      
      
      if (nrow(model_results) > 0) {
        
        all_predictions[[result_number]] <-
          model_results
        
        result_number <- result_number + 1
      }
    }
  }
}


# Combine all prediction results -----------------------------------------

prediction_results <- bind_rows(
  all_predictions
) %>%
  arrange(
    plant,
    training_weeks,
    lag,
    year,
    week
  )


# Summarise model performance =============================================================================


prediction_model_summary <- prediction_results %>%
  
  group_by(
    plant,
    training_weeks,
    lag
  ) %>%
  
  summarise(
    n_predictions = n(),
    
    mspe = mean(
      squared_prediction_error,
      na.rm = TRUE
    ),
    
    n_within_95_pi = sum(
      within_95_pi,
      na.rm = TRUE
    ),
    
    coverage_95 = mean(
      within_95_pi,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    plant,
    training_weeks,
    lag
  )


# Export results =============================================================================


write_csv(
  prediction_results,
  here(
    "results",
    "prediction_results.csv"
  )
)

write_csv(
  prediction_model_summary,
  here(
    "results",
    "prediction_model_summary.csv"
  )
)


# Display summary =============================================================================


prediction_model_summary