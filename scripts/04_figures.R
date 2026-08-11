
# 04_main_figures.R =============================================================================
#
# Reproduces all figures presented in the main manuscript.


# Packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(patchwork)



# # Figure 1: Wastewater SARS-CoV-2 levels and reported COVID-19 c --------

## Import data -------------------------------------------------------------

data_file <- here("data", "Supporting_data_s1.xlsx")

national <- read_excel(
  data_file,
  sheet = "National level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level),
    year = as.integer(sub("-.*", "", year_week)),
    week = as.integer(sub(".*-", "", year_week))
  ) %>%
  arrange(year, week)

stockholm <- read_excel(
  data_file,
  sheet = "Stockholm level"
) %>%
  mutate(
    ww_level = as.numeric(ww_level),
    year = as.integer(sub("-.*", "", year_week)),
    week = as.integer(sub(".*-", "", year_week))
  ) %>%
  arrange(year, week)


## Plot function  =============================================================================

create_time_series_plot <- function(
    data,
    panel_label,
    case_scale
) {
  
  # Preserve chronological order on the discrete x-axis
  data <- data %>%
    mutate(
      year_week = factor(
        year_week,
        levels = unique(year_week)
      )
    )
  
  ggplot(
    data,
    aes(x = year_week)
  ) +
    
    # Reported COVID-19 cases
    geom_line(
      aes(
        y = case / case_scale,
        colour = "COVID-19 cases",
        group = 1
      ),
      linewidth = 1.2
    ) +
    
    # Wastewater SARS-CoV-2 levels
    geom_line(
      aes(
        y = ww_level,
        colour = "SARS-CoV-2 level",
        group = 1
      ),
      linewidth = 1.2,
      linetype = "11"
    ) +
    
    # Panel label
    annotate(
      "text",
      x = 3,
      y = max(data$ww_level, na.rm = TRUE) * 1.1,
      label = panel_label,
      size = 5,
      fontface = "bold",
      hjust = 0
    ) +
    
    # Colours
    scale_colour_manual(
      values = c(
        "COVID-19 cases" = "darkgrey",
        "SARS-CoV-2 level" = "darkred"
      )
    ) +
    
    # Primary and secondary y-axes
    scale_y_continuous(
      name = "Wastewater Levels",
      sec.axis = sec_axis(
        ~ . * case_scale,
        name = "Number of Cases"
      )
    ) +
    
    # Show every third week
    scale_x_discrete(
      breaks = levels(data$year_week)[
        seq(
          1,
          length(levels(data$year_week)),
          by = 3
        )
      ]
    ) +
    
    labs(
      x = "Year-Week",
      colour = NULL
    ) +
    
    theme_minimal() +
    
    theme(
      axis.title.x = element_text(
        size = 12,
        face = "bold"
      ),
      axis.title.y = element_text(
        size = 12,
        face = "bold"
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 10
      ),
      axis.text.y = element_text(
        size = 10
      ),
      axis.line.x = element_line(
        linewidth = 0.8
      ),
      axis.line.y = element_line(
        linewidth = 0.8
      ),
      axis.ticks.x = element_line(
        linewidth = 0.5
      ),
      axis.ticks.y = element_line(
        linewidth = 0.5
      ),
      axis.ticks.length = unit(
        0.25,
        "cm"
      ),
      legend.position = "top",
      legend.text = element_text(
        size = 12
      ),
      panel.grid = element_blank()
    )
}


## Create panels =============================================================================

figure1_stockholm <- create_time_series_plot(
  data = stockholm,
  panel_label = "(A) Stockholm",
  case_scale = 60
)

figure1_national <- create_time_series_plot(
  data = national,
  panel_label = "(B) National",
  case_scale = 300
)


## Combine panels =============================================================================

figure1 <- (
  figure1_stockholm /
    figure1_national
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )


## Display figure ----------------------------------------------------------

figure1


## Export Figure 1=============================================================================

ggsave(
  here(
    "figures",
    "figure1_time_series.tiff"
  ),
  plot = figure1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300,
  compression = "lzw"
)




#  Figure 2: Cross-correlation heatmap =============================================================================


## Import cross-correlation results ----------------------------------------

ccf_results <- read_csv(
  here("results", "cross_correlation_results.csv"),
  show_col_types = FALSE
)


## Import population coverage ---------------------------------------------

regional_coverage <- read_excel(
  here("data", "Supporting_data_s1.xlsx"),
  sheet = "Regional level"
) %>%
  distinct(
    plant,
    population_served,
    pop_region
  ) %>%
  transmute(
    location = plant,
    
    # Whole percentages reproduce the population coverage used
    # in the manuscript.
    population_coverage = round(
      population_served / pop_region * 100
    )
  )

# Add aggregated Stockholm and national population coverage
population_coverage <- bind_rows(
  regional_coverage,
  tibble(
    location = c("Stockholm", "National"),
    population_coverage = c(86, 43)
  )
)


## Prepare Figure 2 data =============================================================================

figure2_data <- ccf_results %>%
  
  # Figure 2 presents only -1, 0 and +1 week
  filter(
    lag %in% c(-1, 0, 1)
  ) %>%
  
  left_join(
    population_coverage,
    by = "location"
  ) %>%
  
  mutate(
    
    # Labels used in the manuscript figure
    period = recode(
      period,
      "Full study period" = "All weeks",
      "Increase" = "Increase",
      "Decrease" = "Decrease"
    ),
    
    period = factor(
      period,
      levels = c(
        "All weeks",
        "Increase",
        "Decrease"
      )
    ),
    
    # Population coverage shown next to each WWTP
    location_label = paste0(
      location,
      " (",
      population_coverage,
      "%)"
    )
  )


## Order WWTPs by population coverage -------------------------------------

# Regional WWTPs are ordered from highest to lowest coverage.
# Stockholm and National are displayed last.

location_order <- population_coverage %>%
  mutate(
    ordering = if_else(
      location %in% c("Stockholm", "National"),
      -Inf,
      as.numeric(population_coverage)
    )
  ) %>%
  arrange(
    desc(ordering)
  ) %>%
  mutate(
    location_label = paste0(
      location,
      " (",
      population_coverage,
      "%)"
    )
  ) %>%
  pull(location_label)

figure2_data <- figure2_data %>%
  mutate(
    location_label = factor(
      location_label,
      levels = rev(location_order)
    )
  )


## Mark highest correlation within each period and location ----------------

figure2_data <- figure2_data %>%
  group_by(
    location,
    period
  ) %>%
  mutate(
    max_correlation = correlation ==
      max(correlation, na.rm = TRUE)
  ) %>%
  ungroup()


## Create x-axis positions -------------------------------------------------

figure2_data <- figure2_data %>%
  mutate(
    x_position = case_when(
      
      period == "All weeks" & lag == -1 ~ 1,
      period == "All weeks" & lag ==  0 ~ 2,
      period == "All weeks" & lag ==  1 ~ 3,
      
      period == "Increase" & lag == -1 ~ 4,
      period == "Increase" & lag ==  0 ~ 5,
      period == "Increase" & lag ==  1 ~ 6,
      
      period == "Decrease" & lag == -1 ~ 7,
      period == "Decrease" & lag ==  0 ~ 8,
      period == "Decrease" & lag ==  1 ~ 9
    )
  )


## Dashed line before Stockholm and National -------------------------------

label_levels <- levels(
  figure2_data$location_label
)

dash_at <- which(
  grepl(
    "^Östhammar",
    label_levels
  )
) - 0.5


## Create Figure 2 ============================================================================

figure2 <- ggplot(
  figure2_data,
  aes(
    x = x_position,
    y = location_label,
    fill = correlation
  )
) +
  
  geom_tile(
    colour = "white",
    linewidth = 0.5,
    alpha = 0.85
  ) +
  
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        correlation
      ),
      fontface = if_else(
        max_correlation,
        "bold",
        "plain"
      )
    ),
    size = 4,
    colour = "black"
  ) +
  
  # Separate full period, increase and decrease
  geom_vline(
    xintercept = c(
      3.5,
      6.5
    ),
    colour = "black",
    linewidth = 1.2
  ) +
  
  # Separate regional WWTPs from Stockholm/National
  geom_hline(
    yintercept = dash_at,
    linetype = "dashed",
    colour = "gray30",
    linewidth = 0.5
  ) +
  
  # Period headings
  annotate(
    "text",
    x = 2,
    y = length(label_levels) + 1.2,
    label = "All weeks",
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = 5,
    y = length(label_levels) + 1.2,
    label = "Increase",
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = 8,
    y = length(label_levels) + 1.2,
    label = "Decrease",
    fontface = "bold",
    size = 5
  ) +
  
  scale_x_continuous(
    breaks = 1:9,
    labels = c(
      "r -1", "r 0", "r +1",
      "r -1", "r 0", "r +1",
      "r -1", "r 0", "r +1"
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  scale_fill_gradient(
    low = "lightyellow",
    high = "tomato",
    limits = c(
      min(
        figure2_data$correlation,
        na.rm = TRUE
      ),
      1
    ),
    name = NULL
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    x = "Correlation at different time lags (weeks)",
    y = "WWTP (population coverage)"
  ) +
  
  theme_minimal(
    base_size = 14
  ) +
  
  theme(
    axis.text.y = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      size = 12
    ),
    axis.title.x = element_text(
      size = 14,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 14
    ),
    legend.text = element_text(
      size = 11
    ),
    panel.grid = element_blank(),
    plot.margin = margin(
      t = 30,
      r = 10,
      b = 20,
      l = 10
    )
  )


## Display Figure 2 --------------------------------------------------------

figure2


## Export Figure 2 =============================================================================

ggsave(
  here(
    "figures",
    "figure2_cross_correlation_heatmap.tiff"
  ),
  plot = figure2,
  width = 7,
  height = 7.5,
  units = "in",
  dpi = 300,
  compression = "lzw"
)





# Figure 3: classification performance =============================================================================


classification <- read_csv(
  here("results", "trend_classification_results.csv"),
  show_col_types = FALSE
)


## Prepare plotting data ---------------------------------------------------

figure3_data <- classification %>%
  mutate(
    trend_type = recode(
      trend_type,
      "Week-to-week" = "Week-to-week",
      "Three-week" = "3-week trends"
    ),
    trend_type = factor(
      trend_type,
      levels = c("Week-to-week", "3-week trends")
    ),
    location = factor(
      location,
      levels = c("Stockholm", "National")
    ),
    threshold = factor(
      threshold,
      levels = c(">10%", ">25%")
    ),
    lag = factor(
      lag,
      levels = c(-3, -2, -1, 0)
    )
  ) %>%
  select(
    location,
    threshold,
    trend_type,
    lag,
    sensitivity,
    sensitivity_lower,
    sensitivity_upper,
    specificity,
    specificity_lower,
    specificity_upper,
    ppv,
    ppv_lower,
    ppv_upper,
    npv,
    npv_lower,
    npv_upper
  ) %>%
  pivot_longer(
    cols = c(
      sensitivity,
      specificity,
      ppv,
      npv
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    lower_ci = case_when(
      metric == "sensitivity" ~ sensitivity_lower,
      metric == "specificity" ~ specificity_lower,
      metric == "ppv" ~ ppv_lower,
      metric == "npv" ~ npv_lower
    ),
    upper_ci = case_when(
      metric == "sensitivity" ~ sensitivity_upper,
      metric == "specificity" ~ specificity_upper,
      metric == "ppv" ~ ppv_upper,
      metric == "npv" ~ npv_upper
    ),
    metric = recode(
      metric,
      sensitivity = "SE",
      specificity = "SP",
      ppv = "PPV",
      npv = "NPV"
    ),
    metric = factor(
      metric,
      levels = c("SE", "SP", "PPV", "NPV")
    ),
    value = value * 100,
    lower_ci = lower_ci * 100,
    upper_ci = upper_ci * 100
  )


## Plot --------------------------------------------------------------------

figure3 <- ggplot(
  figure3_data,
  aes(
    x = lag,
    y = value,
    colour = metric,
    shape = metric
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  facet_grid(
    threshold + trend_type ~ location
  ) +
  labs(
    x = "Lag (weeks)",
    y = "Value (%)",
    colour = NULL,
    shape = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_text(
      hjust = 1
    ),
    panel.spacing = unit(
      1.2,
      "lines"
    ),
    panel.background = element_rect(
      fill = "white",
      colour = "black",
      linewidth = 0.3
    ),
    panel.grid = element_line(
      colour = "gray90"
    ),
    legend.position = "top",
    legend.text = element_text(
      size = 12
    )
  )


## Export ------------------------------------------------------------------

ggsave(
  here("figures", "figure3_classification_performance.tiff"),
  plot = figure3,
  width = 5.5,
  height = 8,
  units = "in",
  dpi = 300,
  compression = "lzw"
)