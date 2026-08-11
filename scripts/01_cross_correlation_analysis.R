
# Cross-correlation analysis =============================================================================
#
# Evaluates the correlation between weekly wastewater SARS-CoV-2 levels
# and reported COVID-19 cases at regional, Stockholm and national level.
#
# Cross-correlations are calculated for lags -3 to +3 weeks for:
#   1. Full study period
#   2. Increasing epidemic period: 2023-W39 to 2023-W45
#   3. Decreasing epidemic period: 2023-W50 to 2024-W04
#
# Lag interpretation when using ccf(ww_level, case):
#   negative lag = wastewater leads reported cases
#   lag 0        = concurrent association
#   positive lag = reported cases lead wastewater




# Packages ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(readr)

str(regional)
str(stockholm)
str(national)
# Import data -------------------------------------------------------------

data_file <- here("data", "Supporting_data_s1.xlsx")

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

regional <- read_excel(
  data_file,
  sheet = "Regional level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level),
    year = as.integer(sub("-.*", "", year_week)),
    week = as.integer(sub(".*-", "", year_week))
  )

# Cross-correlation function ---------------------------------------------

run_ccf <- function(data, name) {
  
  data <- data %>%
    arrange(year, week) %>%
    filter(!is.na(ww_level), !is.na(case))
  
  result <- ccf(
    data$ww_level,
    data$case,
    lag.max = 3,
    plot = FALSE
  )
  
  tibble(
    location = name,
    lag = as.numeric(result$lag),
    correlation = as.numeric(result$acf)
  )
}


# Function for all regional wastewater treatment plants -----------------

run_regional_ccf <- function(data) {
  
  data %>%
    group_split(plant) %>%
    map_dfr(
      ~ run_ccf(
        .x,
        name = unique(.x$plant)
      )
    )
}


# Function for one analysis period ---------------------------------------

analyse_period <- function(regional_data,
                           stockholm_data,
                           national_data,
                           period_name) {
  
  bind_rows(
    run_regional_ccf(regional_data),
    run_ccf(stockholm_data, "Stockholm"),
    run_ccf(national_data, "National")
  ) %>%
    mutate(period = period_name, .before = 1)
}



# 1. Full study period =============================================================================

ccf_full <- analyse_period(
  regional,
  stockholm,
  national,
  period_name = "Full study period"
)


# 2. Increasing epidemic period: week 39-45, 2023 =============================================================================


regional_increase <- regional %>%
  filter(
    year == 2023,
    week >= 39,
    week <= 45
  )

stockholm_increase <- stockholm %>%
  filter(
    year == 2023,
    week >= 39,
    week <= 45
  )

national_increase <- national %>%
  filter(
    year == 2023,
    week >= 39,
    week <= 45
  )

ccf_increase <- analyse_period(
  regional_increase,
  stockholm_increase,
  national_increase,
  period_name = "Increase"
)

# 3. Decreasing epidemic period: week 50, 2023 to week 4, 2024 =============================================================================

regional_decrease <- regional %>%
  filter(
    (year == 2023 & week >= 50) |
      (year == 2024 & week <= 4)
  )

stockholm_decrease <- stockholm %>%
  filter(
    (year == 2023 & week >= 50) |
      (year == 2024 & week <= 4)
  )

national_decrease <- national %>%
  filter(
    (year == 2023 & week >= 50) |
      (year == 2024 & week <= 4)
  )

ccf_decrease <- analyse_period(
  regional_decrease,
  stockholm_decrease,
  national_decrease,
  period_name = "Decrease"
)


# Combine result =============================================================================

ccf_results <- bind_rows(
  ccf_full,
  ccf_increase,
  ccf_decrease
)


# Wide version for tables -------------------------------------------------

ccf_results_wide <- ccf_results %>%
  pivot_wider(
    names_from = lag,
    values_from = correlation,
    names_prefix = "lag_"
  )



# Export results =============================================================================

write_csv(
  ccf_results,
  here("results", "cross_correlation_results.csv")
)

write_csv(
  ccf_results_wide,
  here("results", "cross_correlation_results_wide.csv")
)



# Association between correlation and population coverage ==================



coverage_analysis <- regional %>%
  distinct(
    plant,
    population_served,
    pop_region
  ) %>%
  mutate(
    population_coverage = round(population_served / pop_region * 100)
  ) %>%
  select(
    plant,
    population_coverage
  ) %>%
  left_join(
    ccf_full %>%
      filter(
        lag == 0,
        !location %in% c("Stockholm", "National")
      ) %>%
      select(
        plant = location,
        correlation
      ),
    by = "plant"
  )


spearman_coverage <- cor.test(
  coverage_analysis$correlation,
  coverage_analysis$population_coverage,
  method = "spearman",
  exact = FALSE
)

coverage <- regional %>%
  distinct(
    plant,
    population_served,
    pop_region
  ) %>%
  mutate(
    population_coverage = round(
      population_served / pop_region * 100
    )
  )

spearman_coverage

# Save Spearman results
capture.output(
  spearman_coverage,
  file = here("results", "spearman_population_coverage.txt")
)

