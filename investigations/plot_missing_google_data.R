# Visualize Google data missingness at US county level

library(data.table)
library(ggplot2)
library(scales)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# define path
input_path <- "/mnt/team/covid_19/pub/model-inputs/2022_12_13.03/mobility/google_mobility_with_locs.csv"
out_path <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"

# location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
cnty_totals <- hierarchy[level==3, length(unique(location_id)), by=parent_id]
setnames(cnty_totals, c('V1','parent_id'), c('total_counties','location_id'))
cnty_totals <- merge(cnty_totals, hierarchy[level==2, .(location_id, location_name)], by='location_id')

# Load the data
vars_to_keep <- c('location_id', 'location_name', 'date', 'country_region', 'sub_region_1', 'sub_region_2',
                  'retail_and_recreation_percent_change_from_baseline', 'retail_imputed')
dt <- fread("/mnt/team/covid_19/pub/model-inputs/2022_12_13.03/mobility/google_mobility_with_locs.csv")[, .SD, .SDcols = vars_to_keep]
dt[, date:=as.Date(date, format = "%d.%m.%Y")]

# Subset to US Counties
US_counties <- dt[country_region %in% 'United States' & sub_region_2!="" & date < '2020-12-31']

# Enumerate # of counties per state that are missing 
cnty_summary <- US_counties[, length(unique(sub_region_2)), by=sub_region_1]
setnames(cnty_summary, c('V1','sub_region_1'), c('n_counties','location_name'))
cnty_summary <- merge(cnty_summary, cnty_totals, by='location_name')
cnty_summary[, pct_missing := round(((total_counties - n_counties)/total_counties)*100,1)]
cnty_sorted <- cnty_summary[order(pct_missing)]
fwrite(cnty_sorted, paste0(out_path,'Google_missing_counties.csv'))



# Visualize the days that are missing 
dt <- copy(US_counties)[, .(location_id, county = sub_region_2, state = sub_region_1, date, value = retail_and_recreation_percent_change_from_baseline)]

# (1) Make the dataset square
all_locations <- unique(dt[, .(county, state)])
all_dates     <- data.table(date = seq(min(dt$date), max(dt$date), by = "day"))

# Cross join: every location x every date
square <- all_locations[, CJ(date = all_dates$date), by = .(county, state)]

# Left join observed data
square <- dt[square, on = .(county, state, date)]

# Missingness indicator
square[, is_missing := is.na(value)]

# (2) Plot 1: Histogram of % of days missing per county
county_miss <- square[, .(pct_missing = mean(is_missing) * 100), by = .(county, state)]
 
p1 <- ggplot(county_miss, aes(x = pct_missing)) +
  geom_histogram(binwidth = 2, fill = "#2c7bb6", color = "white", linewidth = 0.2) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = label_percent(scale = 1),
    breaks = seq(0, 100, 20)
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Distribution of missingness across counties",
    x        = "% of days missing",
    y        = "Number of counties"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
 
print(p1)

# (3) plot 2: line plot of % of counties missing per day
day_miss <- square[, .(pct_missing = mean(is_missing) * 100), by = date]

p2 <- ggplot(day_miss, aes(x = date, y = pct_missing)) +
  geom_line(color = "#d7191c", linewidth = 0.7) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    limits = c(0, NA),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title    = "% of counties missing data on each day",
    x        = NULL,
    y        = "% counties missing"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p2)

# (4) plot 3: heat map
# For each state-day: % of counties in that state that are missing.
# States sorted by overall missingness (highest at top).

state_day_miss <- square[,
                         .(pct_missing = mean(is_missing) * 100),
                         by = .(state, date)
]

# Order states by overall missingness (for y-axis)
state_order <- state_day_miss[, .(mean_miss = mean(pct_missing)), by = state]
setorder(state_order, -mean_miss)
state_day_miss[, state := factor(state, levels = state_order$state)]

p3 <- ggplot(state_day_miss, aes(x = date, y = state, fill = pct_missing)) +
  geom_tile() +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    limits   = c(0, 100),
    labels   = label_percent(scale = 1),
    name     = "% counties\nmissing"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title    = "State × day missingness heatmap",
    subtitle = "States ordered by overall % missing (top = most missing); color = % of counties in state with missing data",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    axis.text.y      = element_text(size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid       = element_blank(),
    legend.key.height = unit(1.5, "cm")
  )

print(p3)
