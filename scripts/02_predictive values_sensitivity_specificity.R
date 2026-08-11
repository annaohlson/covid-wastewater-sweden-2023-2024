# 02_predictive values_sensitivity_specificity.R =============================================================================
# 
#
# Evaluates how well changes in wastewater SARS-CoV-2 levels correspond
# to subsequent changes in reported COVID-19 cases at Stockholm and
# national level.
#
# Two definitions of change are evaluated:
# - Week-to-week change
# - Rolling three-week trend
#
# Wastewater changes are evaluated at:
# - No time shift (lag 0)
# - 1-week lead (lag -1)
# - 2-week lead (lag -2)
# - 3-week lead (lag -3)
#
# Negative lags indicate that wastewater precedes reported cases,
# reflecting the evaluation of wastewater surveillance as an early
# warning indicator.
#
# Changes are classified using thresholds of:
# - >10%
# - >25%
#
# Sensitivity, specificity, positive predictive value (PPV), and negative
# predictive value (NPV) are calculated using reported COVID-19 cases as
# the reference surveillance indicator.
#
# Weeks with low wastewater levels are excluded using the 20th percentile
# cut-offs applied in the original analysis:
# - National: 0.18
# - Stockholm: 0.19
#
# 95% confidence intervals are calculated using the normal approximation,
# reproducing the original analysis.



# Packages ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)


# Import data =============================================================================


data_file <- here("data", "Supporting_data_s1.xlsx")

national <- read_excel(
  data_file,
  sheet = "National level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level)
  )

stockholm <- read_excel(
  data_file,
  sheet = "Stockholm level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level)
  )

#fix year-week variable
national <- read_excel(
  data_file,
  sheet = "National level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level),
    year = as.integer(sub("-.*", "", year_week)),
    week = as.integer(sub(".*-", "", year_week))
  )

stockholm <- read_excel(
  data_file,
  sheet = "Stockholm level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level),
    year = as.integer(sub("-.*", "", year_week)),
    week = as.integer(sub(".*-", "", year_week))
  )

# Week-to-week change =============================================================================


calculate_week_to_week <- function(data, name) {
  
  data <- data %>%
    arrange(year, week) %>%
    mutate(
      week_index = row_number(),
      
      # Replace zero values before log10 transformation
      ww_log = log10(
        if_else(
          ww_level == 0,
          .Machine$double.eps,
          ww_level
        )
      )
    )
  
  map_dfr(
    seq(2, nrow(data)),
    function(end_week) {
      
      # Two-week window representing week-to-week change
      window <- data[
        (end_week - 1):end_week,
      ]
      
      # Poisson model for reported cases
      model_case <- glm(
        case ~ week_index,
        family = poisson(link = "log"),
        data = window
      )
      
      case_change <- (
        exp(coef(model_case)[["week_index"]]) - 1
      ) * 100
      
      
      # Linear model for wastewater levels
      model_ww <- lm(
        ww_log ~ week_index,
        data = window
      )
      
      ww_change <- (
        10^coef(model_ww)[["week_index"]] - 1
      ) * 100
      
      
      tibble(
        location = name,
        year_week = data$year_week[end_week],
        ww_level = data$ww_level[end_week],
        case_change = case_change,
        ww_change = ww_change
      )
    }
  )
}


# Calculate week-to-week changes -----------------------------------------

week_to_week <- bind_rows(
  
  calculate_week_to_week(
    stockholm,
    name = "Stockholm"
  ),
  
  calculate_week_to_week(
    national,
    name = "National"
  )
  
) %>%
  mutate(
    trend_type = "Week-to-week"
  )


# Three-week trend =============================================================================


calculate_three_week_trend <- function(data, name) {
  
  data <- data %>%
    arrange(year, week) %>%
    mutate(
      week_index = row_number(),
      
      # Replace zero values before log10 transformation
      ww_log = log10(
        if_else(
          ww_level == 0,
          .Machine$double.eps,
          ww_level
        )
      )
    )
  
  map_dfr(
    seq(3, nrow(data)),
    function(end_week) {
      
      # Rolling three-week window
      window <- data[
        (end_week - 2):end_week,
      ] %>%
        filter(
          !is.na(ww_log),
          !is.na(case)
        )
      
      # At least two observations are required to estimate a slope
      if (nrow(window) < 2) {
        
        return(
          tibble(
            location = name,
            year_week = data$year_week[end_week],
            ww_level = data$ww_level[end_week],
            case_change = NA_real_,
            ww_change = NA_real_
          )
        )
      }
      
      
      # Poisson model for reported cases
      model_case <- glm(
        case ~ week_index,
        family = poisson(link = "log"),
        data = window
      )
      
      case_change <- (
        exp(coef(model_case)[["week_index"]]) - 1
      ) * 100
      
      
      # Linear model for wastewater levels
      model_ww <- lm(
        ww_log ~ week_index,
        data = window
      )
      
      ww_change <- (
        10^coef(model_ww)[["week_index"]] - 1
      ) * 100
      
          tibble(
          location = name,
          year_week = data$year_week[end_week],
          year = data$year[end_week],
          week = data$week[end_week],
          ww_level = data$ww_level[end_week],
          case_change = case_change,
          ww_change = ww_change
        )
      
    }
  )
}


# Calculate three-week trends --------------------------------------------

three_week <- bind_rows(
  
  calculate_three_week_trend(
    stockholm,
    name = "Stockholm"
  ),
  
  calculate_three_week_trend(
    national,
    name = "National"
  )
  
) %>%
  mutate(
    trend_type = "Three-week"
  )


# Combine trend definitions =============================================================================


trend_data <- bind_rows(
  week_to_week,
  three_week
)


# Exclude weeks with low wastewater levels =============================================================================


# Cut-offs correspond to the 20th percentile of wastewater levels used
# in the original analysis.

trend_data <- trend_data %>%
  mutate(
    ww_cutoff = case_when(
      location == "National"  ~ 0.18,
      location == "Stockholm" ~ 0.19
    ),
    
    # Exclude change estimates when wastewater level is below the cut-off
    ww_change = if_else(
      ww_level < ww_cutoff,
      NA_real_,
      ww_change
    )
  )


# Functions for sensitivity, specificity, PPV and NPV  =============================================================================



# 95% confidence interval -------------------------------------------------
#
# Uses the normal approximation applied in the original analysis.

calculate_ci <- function(events, total) {
  
  if (total == 0) {
    return(
      c(
        NA_real_,
        NA_real_
      )
    )
  }
  
  p <- events / total
  z <- qnorm(0.975)
  
  lower <- p - z * sqrt(
    p * (1 - p) / total
  )
  
  upper <- p + z * sqrt(
    p * (1 - p) / total
  )
  
  # Restrict confidence limits to 0-1
  lower <- max(
    0,
    lower
  )
  
  upper <- min(
    1,
    upper
  )
  
  c(
    lower,
    upper
  )
}


# Calculate classification metrics ---------------------------------------

calculate_metrics <- function(case_positive, ww_positive) {
  
  # Keep observations where both classifications are available
  complete <- (
    !is.na(case_positive) &
      !is.na(ww_positive)
  )
  
  case_positive <- case_positive[complete]
  ww_positive <- ww_positive[complete]
  
  
  # Confusion matrix
  tp <- sum(
    case_positive &
      ww_positive
  )
  
  fp <- sum(
    !case_positive &
      ww_positive
  )
  
  fn <- sum(
    case_positive &
      !ww_positive
  )
  
  tn <- sum(
    !case_positive &
      !ww_positive
  )
  
  
  # Performance metrics
  sensitivity <- ifelse(
    tp + fn > 0,
    tp / (tp + fn),
    NA_real_
  )
  
  specificity <- ifelse(
    tn + fp > 0,
    tn / (tn + fp),
    NA_real_
  )
  
  ppv <- ifelse(
    tp + fp > 0,
    tp / (tp + fp),
    NA_real_
  )
  
  npv <- ifelse(
    tn + fn > 0,
    tn / (tn + fn),
    NA_real_
  )
  
  
  # Confidence intervals
  sensitivity_ci <- calculate_ci(
    tp,
    tp + fn
  )
  
  specificity_ci <- calculate_ci(
    tn,
    tn + fp
  )
  
  ppv_ci <- calculate_ci(
    tp,
    tp + fp
  )
  
  npv_ci <- calculate_ci(
    tn,
    tn + fn
  )
  
  
  tibble(
    n = tp + fp + fn + tn,
    
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    
    sensitivity = sensitivity,
    sensitivity_lower = sensitivity_ci[1],
    sensitivity_upper = sensitivity_ci[2],
    
    specificity = specificity,
    specificity_lower = specificity_ci[1],
    specificity_upper = specificity_ci[2],
    
    ppv = ppv,
    ppv_lower = ppv_ci[1],
    ppv_upper = ppv_ci[2],
    
    npv = npv,
    npv_lower = npv_ci[1],
    npv_upper = npv_ci[2]
  )
}


# Classification analysis =============================================================================


run_classification <- function(data, threshold, ww_lead_weeks) {
  
  data <- data %>%
    arrange(year, week) %>%
    
    # Exclude missing trend estimates before creating lead values,
    # reproducing the original analysis.
    filter(
      !is.na(case_change),
      !is.na(ww_change)
    ) %>%
    
    mutate(
      
      # A positive value of ww_lead_weeks means that wastewater is taken
      # from that many weeks earlier than the corresponding case value.
      ww_leading = dplyr::lag(
        ww_change,
        n = ww_lead_weeks
      ),
      
      # Define increases according to threshold
      case_positive = case_change > threshold,
      ww_positive = ww_leading > threshold
    )
  
  
  calculate_metrics(
    case_positive = data$case_positive,
    ww_positive = data$ww_positive
  ) %>%
    
    mutate(
      threshold = threshold,
      
      # Export using the manuscript convention:
      # 0 = same week; negative values = wastewater leads cases.
      lag = -ww_lead_weeks,
      
      .before = 1
    )
}


# Run all thresholds and wastewater leads --------------------------------

run_all_classifications <- function(data) {
  
  crossing(
    threshold = c(10, 25),
    ww_lead_weeks = 0:3
  ) %>%
    
    pmap_dfr(
      function(threshold, ww_lead_weeks) {
        
        run_classification(
          data = data,
          threshold = threshold,
          ww_lead_weeks = ww_lead_weeks
        )
      }
    )
}


#  Run analyses =============================================================================


classification_results <- trend_data %>%
  
  group_by(
    trend_type,
    location
  ) %>%
  
  group_split() %>%
  
  map_dfr(
    function(data) {
      
      trend_name <- unique(
        data$trend_type
      )
      
      location_name <- unique(
        data$location
      )
      
      
      run_all_classifications(
        data
      ) %>%
        
        mutate(
          trend_type = trend_name,
          location = location_name,
          .before = 1
        )
    }
  ) %>%
  
  mutate(
    threshold = paste0(
      ">",
      threshold,
      "%"
    )
  ) %>%
  
  arrange(
    trend_type,
    threshold,
    location,
    lag
  )





# Display key results =============================================================================


classification_results %>%
  filter(
    lag == 0
  ) %>%
  select(
    trend_type,
    location,
    threshold,
    sensitivity,
    specificity,
    ppv,
    npv
  )


# Round values 
classification_results_round <- classification_results %>%
  mutate(
    across(
      c(
        sensitivity,
        specificity,
        ppv,
        npv,
        sensitivity_lower,
        sensitivity_upper,
        specificity_lower,
        specificity_upper,
        ppv_lower,
        ppv_upper,
        npv_lower,
        npv_upper
      ),
      ~ round(.x, 3)
    )
  )




# Export results =============================================================================


write_csv(
  classification_results_round,
  here(
    "results",
    "trend_classification_results.csv"
  )
)