# Generate a 4-panel county-level plot for every state and save all to a PDF

pdf("state_county_plots.pdf", width = 14, height = 10)

# Get all state-level location IDs from the hierarchy
state_ids <- unique(hierarchy[level == "state", location_id])

for (state_id in state_ids) {

  # Subset to counties in the state
  state_counties <- hierarchy[parent_id == state_id, location_id]

  # Skip states with no county children
  if (length(state_counties) == 0) next

  state_dt <- all_weekly[location_id %in% state_counties]

  # Skip if no data for this state
  if (nrow(state_dt) == 0) next

  # Get state name for the plot title
  state_name <- hierarchy[location_id == state_id, location_name]

  # Get population by county
  pop_by_county <- unique(full_data[location_id %in% state_counties, .(location_id, pop)])

  # Skip if population data is missing
  if (nrow(pop_by_county) == 0) next

  pop_by_county[, county_size := fcase(
    pop >= quantile(pop, 0.95), "big",
    pop <= quantile(pop, 0.05), "small",
    default = "mid"
  )]

  # Identify counties with outlier populations: log(pop) outside 1.5*IQR
  pop_by_county[, log_pop := log(pop)]
  pop_by_county[, c("q1", "q3") := .(
    quantile(log_pop, 0.25),
    quantile(log_pop, 0.75)
  )]
  pop_by_county[, iqr := q3 - q1]
  pop_by_county[, size_IQR := fcase(
    log_pop < (q1 - 1.5 * iqr), "small",
    log_pop > (q3 + 1.5 * iqr), "big",
    default = "mid"
  )]

  # Clean up intermediate columns
  pop_by_county[, c("log_pop", "q1", "q3", "iqr") := NULL]

  # Merge population size classification into state data
  state_dt <- merge(state_dt, pop_by_county, by = "location_id", all.x = TRUE)

  # Calculate state means across all counties
  state_means <- state_dt[, .(
    visits   = mean(y),
    norm     = mean(y / mean_jan_feb),
    log_norm = mean(y_norm),
    delta    = mean(delta)
  ), by = "time_id"]

  colors <- c("#4DAF4A", "#377EB8")

  p1 <- ggplot() +
    geom_line(data = state_dt,
              aes(x = time_id, y = y, group = as.factor(location_id)),
              alpha = 0.3) +
    geom_line(data = state_dt[size_IQR != "mid"],
              aes(x = time_id, y = y, group = as.factor(location_id), color = size_IQR),
              size = 1) +
    geom_line(data = state_means,
              aes(x = time_id, y = visits),
              color = "red", size = 1.5) +
    scale_x_continuous(name = "Weeks til mandate", breaks = c(-3, -2, -1), labels = c(3, 2, 1)) +
    scale_y_continuous(name = "Visits per 10K") +
    scale_color_manual(name = "County Size", values = colors) +
    theme_classic()

  p2 <- ggplot() +
    geom_line(data = state_dt,
              aes(x = time_id, y = y / mean_jan_feb, group = as.factor(location_id)),
              alpha = 0.3) +
    geom_line(data = state_dt[size_IQR != "mid"],
              aes(x = time_id, y = y / mean_jan_feb, group = as.factor(location_id), color = size_IQR),
              size = 1) +
    geom_line(data = state_means,
              aes(x = time_id, y = norm),
              color = "red", size = 1.5) +
    scale_x_continuous(name = "Weeks til mandate", breaks = c(-3, -2, -1), labels = c(3, 2, 1)) +
    scale_y_continuous(name = "Normalized visits per 10K") +
    scale_color_manual(name = "County Size", values = colors) +
    theme_classic()

  p3 <- ggplot() +
    geom_line(data = state_dt,
              aes(x = time_id, y = y_norm, group = as.factor(location_id)),
              alpha = 0.3) +
    geom_line(data = state_dt[size_IQR != "mid"],
              aes(x = time_id, y = y_norm, group = as.factor(location_id), color = size_IQR),
              size = 1) +
    geom_line(data = state_means,
              aes(x = time_id, y = log_norm),
              color = "red", size = 1.5) +
    scale_x_continuous(name = "Weeks til mandate", breaks = c(-3, -2, -1), labels = c(3, 2, 1)) +
    scale_y_continuous(name = "Logged normalized visits per 10K") +
    scale_color_manual(name = "County Size", values = colors) +
    theme_classic()

  p4 <- ggplot() +
    geom_line(data = state_dt,
              aes(x = time_id, y = delta, group = as.factor(location_id)),
              alpha = 0.3) +
    geom_line(data = state_dt[size_IQR != "mid"],
              aes(x = time_id, y = delta, group = as.factor(location_id), color = size_IQR),
              size = 1) +
    geom_line(data = state_means,
              aes(x = time_id, y = delta),
              color = "red", size = 1.5) +
    scale_x_continuous(name = "Weeks til mandate", breaks = c(-3, -2, -1), labels = c(3, 2, 1)) +
    scale_y_continuous(name = "Change in logged normalized visits per 10K") +
    scale_color_manual(name = "County Size", values = colors) +
    theme_classic()

  combined <- p1 + p2 + p3 + p4 +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = state_name)

  print(combined)
}

dev.off()
