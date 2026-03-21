# Calculate the "empirical sd" statistic for SafeGraph data
# for all 50 US states

# Load packages
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(patchwork)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# simulation results version
version <- '20260224.01'

# Define file paths
input_data_path <- '/ihme/scratch/users/ems2285/thesis/intermediate/visits_by_cnty_day_cat_v3.csv'
output_path     <- '/ihme/scratch/users/ems2285/thesis/intermediate/empirical_sd_by_state.csv'

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Get all US states
us_states <- hierarchy[parent_id == 102 & level==2, .(location_id, location_name)]

# Load full input data once
full_data <- fread(input_data_path)

# ---------------------------------------------------------------------------
# Function: calculate empirical_sd for a single state
# Returns a list: empirical_sd, n_counties_total, n_counties_incomplete
# ---------------------------------------------------------------------------
calc_empirical_sd <- function(state_location_id, return_type=c("list","table")) {
  return_type <- match.arg(return_type)

  # Counties belonging to this state
  locs_dt <- hierarchy[parent_id == state_location_id, .(location_id, location_name)]
  n_total <- nrow(locs_dt)
  incomplete <- function(n_complete) list(empirical_sd = NA_real_, n_counties_total = n_total,
                                          n_counties_incomplete = n_total - n_complete)
  if (n_total == 0) return(incomplete(0))

  # Determine date of first mandate imposition by county
  events_dt <- full_data[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                         .(location_id, date = as.Date(date), mandate = dining_close)][order(location_id, date)]
  events_dt[, lag := shift(mandate, fill = 0), by = location_id]
  events_dt <- events_dt[mandate != lag]
  events_dt <- events_dt[, .SD[1], by = location_id]
  if (nrow(events_dt) == 0) return(incomplete(0))

  # Load mobility data up to the last mandate date in the state
  mobility_dt <- full_data[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                           .(location_id, date = as.Date(date), y = visits_per_10k,
                             mandate = dining_close)][date < max(events_dt$date)]

  # --- Baseline: first 8 weeks of 2020 (56 days per county) ---
  baseline_dt <- mobility_dt[date >= '2020-01-01' & date < '2020-02-26']
  complete_baseline <- baseline_dt[!is.na(y), .N, by = location_id][N == 56, location_id]
  if (length(complete_baseline) == 0) return(incomplete(0))

  # subset to locations with no missing data during the 56-day baseline period
  baseline_dt <- baseline_dt[location_id %in% complete_baseline]
  # aggregate to the week level
  baseline_dt[, week_id := rep(1:8, each = 7), by = location_id]
  baseline_weekly <- baseline_dt[, .(y = sum(y)), by = c('location_id', 'week_id')]
  # calculate the mean value over the baseline period for each location
  baseline_weekly <- baseline_weekly[, .(mean_jan_feb = mean(y)), by = location_id]

  # --- Pre-mandate window: 4 weeks (28 days) before each county's mandate ---
  mobility_dt <- merge(mobility_dt, events_dt[, .(location_id, onset = date)], by='location_id')
  mobility_dt <- mobility_dt[date >= (onset - 28) & date < onset]

  # Keep only counties with complete pre-mandate window AND complete baseline
  complete_locs <- intersect(
    mobility_dt[! is.na(y), .N, by = location_id][N == 28, location_id],
    complete_baseline
  )
  if (length(complete_locs) == 0) return(incomplete(0))
  mobility_dt <- mobility_dt[location_id %in% complete_locs]

  # Summarize to weekly level
  mobility_dt[, time_id := rep(-4:(-1), each = 7), by = location_id]
  weekly_dt <- mobility_dt[, .(y = sum(y)), by = c('location_id', 'time_id')]

  # Normalize by Jan-Feb baseline then convert to log space
  weekly_dt <- merge(weekly_dt, baseline_weekly, by = 'location_id')
  weekly_dt[, y_norm := log(y / mean_jan_feb)] 

  # Week-to-week first differences
  weekly_dt[, delta := y_norm - shift(y_norm, type = 'lag'), by = location_id]
  weekly_dt <- weekly_dt[!is.na(delta)]
  if (nrow(weekly_dt) == 0) return(incomplete(0))
  
  if(return_type=='table'){
    return(weekly_dt)
  }
  
  list(
    empirical_sd          = sd(weekly_dt$delta),
    n_counties_total      = n_total,
    n_counties_incomplete = n_total - length(complete_locs)
  )

}

# ---------------------------------------------------------------------------
# Run for all states
# ---------------------------------------------------------------------------
results <- rbindlist(lapply(seq_len(nrow(us_states)), function(i) {
  sid   <- us_states$location_id[i]
  sname <- us_states$location_name[i]
  message(sprintf("[%d/%d] %s", i, nrow(us_states), sname))
  res <- tryCatch(
    calc_empirical_sd(sid, "list"),
    error = function(e) {
      message(sprintf("  ERROR: %s", e$message))
      list(empirical_sd = NA_real_, n_counties_total = NA_integer_, n_counties_incomplete = NA_integer_)
    }
  )
  data.table(location_id = sid, location_name = sname, empirical_sd = res$empirical_sd,
             n_counties_total = res$n_counties_total, n_counties_incomplete = res$n_counties_incomplete)
}))

# Save results
fwrite(results, output_path)
message(sprintf("Done. Results for %d states saved to:\n  %s", nrow(results), output_path))


# Translate the empirical sd and number of counties to a minimum effect size


# Make a map of the minimum effect size per state
dt <- fread(file.path(output_path))

# define bins of SD
dt[, sd_discr := ifelse(empirical_sd<0.1, '<0.1',
                        ifelse(empirical_sd<0.15, '[0.1,0.15)',
                          ifelse(empirical_sd<0.2, '[0.15,0.2)',
                               ifelse(empirical_sd<0.25, '[0.2,0.25)',
                                   ifelse(empirical_sd<0.3, '[0.25,0.3)', '>=0.3')))))]
dt[, sd_discr_2 := factor(sd_discr,
                          levels = c('<0.1','[0.1,0.15)','[0.15,0.2)','[0.2,0.25)','[0.25,0.3)','>=0.3'))]

# define colors
COLS <- c(brewer.pal(6,"YlGnBu"),"#808080")

# load and prep the shape file
map_A1_data <- st_read("/snfs1/WORK/11_geospatial/admin_shapefiles/2024_07_29/lbd_standard_admin_1.shp")
map_A1_data <- map_A1_data[map_A1_data$ADM0_NAME == "United States",]
map_A1_data_contiguous <- map_A1_data[-which(map_A1_data$ADM1_NAME %in% c("Alaska", "Hawaii")),]
# add the data
map_A1_data_contiguous <- merge(map_A1_data_contiguous, dt, by.x='loc_id', by.y='location_id', all.x=T)

sd_plot <- ggplot(data = map_A1_data_contiguous) +
  geom_sf(aes(geometry = geometry, fill = sd_discr_2)) + 
  scale_fill_manual(name = "Pre-Mandate Volatility", values=COLS) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())



# Calculate the minimum detectable effect by state
# load sim results
sim_dt <- fread(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/snr_vs_L.csv'))

# 1. for each location, look up the smallest SNR_new where sim_dt$L < (n_counties_total - n_counties_incomplete)
dt[, n_counties_QC := n_counties_total - n_counties_incomplete]
small_dt <- dt[, .(location_id, empirical_sd, n_counties_QC)]

# Cross join, filter to rows where requirement is met, then take minimum SNR per location
cross <- small_dt[rep(1:.N, each = nrow(sim_dt))]
cross[, SNR := rep(sim_dt$SNR_new, times = nrow(small_dt))]
cross[, required_L := rep(sim_dt$L, times = nrow(small_dt))]

result <- cross[required_L <= n_counties_QC
][, .SD[which.min(SNR)], by = location_id]


# 2. calc effect size (SNR * noise = effect size)
result[, theta := SNR * empirical_sd]
result[, exp_theta := exp(theta)]
result[, eff_discr := ifelse(exp_theta<1.04, '<4%',
                             ifelse(exp_theta<1.06, '[4%-6%)',
                                    ifelse(exp_theta<1.08, '[6%-8%)',
                                           ifelse(exp_theta<1.10, '[8%-10%)', '>=10%'))))]
result[, eff_discr := factor(eff_discr,
                             levels = c('<4%', '[4%-6%)', '[6%-8%)', '[8%-10%)', '>=10%'))]


# plot
COLS2 <- c(brewer.pal(5,"YlGnBu"),"#808080")
map_A1_data_contiguous <- merge(map_A1_data_contiguous, result[,.(location_id, eff_discr)],
                                by.x='loc_id', by.y='location_id', all.x=T)

eff_size_plot <- ggplot(data = map_A1_data_contiguous) +
  geom_sf(aes(geometry = geometry, fill = eff_discr)) + 
  scale_fill_manual(name = "% Change in Visits per 10K", values=COLS2) +
  ggtitle("Minimum Detectable Change in Visits per 10K Population") +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


pdf(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/safegraph_maps.pdf'), width=7, height=5)
sd_plot
eff_size_plot
dev.off()


#####
# Make some plots to vet the assumptions involved with combining all counties in a state
# (Run lines 1-103 and then skip down here)
all_weekly <- rbindlist(lapply(seq_len(nrow(us_states)), function(i) {
  sid   <- us_states$location_id[i]
  sname <- us_states$location_name[i]
  message(sprintf("[%d/%d] %s", i, nrow(us_states), sname))
  res <- tryCatch(
    calc_empirical_sd(sid, return_type = "table"),
    error = function(e) {
      message(sprintf("  ERROR: %s", e$message))
      NULL
    }
  )
  if (!is.data.table(res)) return(NULL)   # catches both errors and incomplete() returns
  res[, `:=`(state_location_id = sid, state_name = sname)]
}), fill = TRUE)



# 1. visits per 10K pop vs deviation of delta y from the mean delta
all_weekly[, mean_delta := mean(delta), by=state_location_id]
all_weekly[, D := delta - mean_delta]

pdf(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/visits_vs_D_all_in_one.pdf'), width=15, height=11)
ggplot(all_weekly[y<=5000], aes(x=y, y=D)) +
  geom_point(alpha=0.2) +
  geom_hline(yintercept = 0, linetype='dashed') +
  theme_classic() +
  facet_wrap(~state_name)
dev.off()

pdf(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/visits_vs_D_by_state.pdf'), width=7, height=5)
for(state in unique(all_weekly$state_name)){
  p <- ggplot(all_weekly[state_name==state], aes(x=y, y=D)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_x_continuous(name='Restaurant visits per 10K population') +
    ggtitle(state) +
    theme_classic()
  print(p)
}
dev.off()

# 2. county population vs deviation of delta y from the mean delta
pop_by_county <- unique(full_data[,.(location_id, pop)])
all_weekly <- merge(all_weekly, pop_by_county, by='location_id', all.x=T)

pdf(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/pop_vs_D_by_state.pdf'), width=7, height=5)
for(state in unique(all_weekly$state_name)){
  p <- ggplot(all_weekly[state_name==state], aes(x=pop, y=D)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype='dashed') +
    scale_x_continuous(name='County Population') +
    ggtitle(state) +
    theme_classic()
  print(p)
}
dev.off()




pdf(paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/',version,'/state_vetting_plots.pdf'), width=14, height=10)

# get all state-level location ids
state_ids <- unique(all_weekly$state_location_id)

for (state_id in state_ids){
  # Subset to counties in the state
  counties <- hierarchy[parent_id==state_id, location_id]
  state_dt <- all_weekly[location_id %in% counties]
  
  # Get state name for plot title
  state_name <- hierarchy[location_id==state_id, location_name]

  # Get population by county and identify top and bottom 5%
  pop_by_county <- unique(full_data[location_id %in% counties,.(location_id, pop)])
  

  # Identify counties with outlier populations: log(pop) is outside the IQR
  pop_by_county[, log_pop := log(pop)]
  pop_by_county[, c("q1", "q3") := .(
    quantile(log_pop, 0.25),
    quantile(log_pop, 0.75))]
  pop_by_county[, iqr := q3 - q1]
  pop_by_county[, size_IQR := fcase(
    log_pop < (q1 - 1.5 * iqr), "small",
    log_pop > (q3 + 1.5 * iqr), "big",
    default = "mid")]

  # Clean up intermediate columns
  pop_by_county[, c("log_pop", "q1", "q3", "iqr") := NULL]

  # Merge pop classification onto SG visit data
  state_dt <- merge(state_dt, pop_by_county, by='location_id', all.x=T)

  # Calculate state means across all counties
  state_means <- state_dt[, .(visits = mean(y),
                                norm = mean(y/mean_jan_feb),
                                log_norm = mean(y_norm),
                                delta = mean(delta)), by='time_id']
  # Define colors
  colors <- c("#4DAF4A", "#377EB8")

  # Plot
  p1 <- ggplot() +
    geom_line(data=state_dt, aes(x=time_id, y=y, group=as.factor(location_id)), alpha=0.3) +
    geom_line(data=state_dt[size_IQR != 'mid'], aes(x=time_id, y=y, group=as.factor(location_id), color=size_IQR), size=1) +
    geom_line(data=state_means, aes(x=time_id, y=visits), color='red', size=1.5) +
    scale_x_continuous(name="Weeks til mandate", breaks=c(-3,-2,-1), labels=c(3,2,1)) +
    scale_y_continuous(name="Visits per 10K") +
    scale_color_manual(name='County Size', values=colors) +
    theme_classic() 

  p2 <- ggplot() +
    geom_line(data=state_dt, aes(x=time_id, y=y/mean_jan_feb, group=as.factor(location_id)), alpha=0.3) +
    geom_line(data=state_dt[size_IQR != 'mid'], aes(x=time_id, y=y/mean_jan_feb, group=as.factor(location_id), color=size_IQR), size=1) +
    geom_line(data=state_means, aes(x=time_id, y=norm), color='red', size=1.5) +
    scale_x_continuous(name="Weeks til mandate", breaks=c(-3,-2,-1), labels=c(3,2,1)) +
    scale_y_continuous(name="Normalized visits per 10K") +
    scale_color_manual(name='County Size', values=colors) +
    theme_classic()  

  p3 <- ggplot() +
    geom_line(data=state_dt, aes(x=time_id, y=y_norm, group=as.factor(location_id)), alpha=0.3) +
    geom_line(data=state_dt[size_IQR != 'mid'], aes(x=time_id, y=y_norm, group=as.factor(location_id), color=size_IQR), size=1) +
    geom_line(data=state_means, aes(x=time_id, y=log_norm), color='red', size=1.5) +
    scale_x_continuous(name="Weeks til mandate", breaks=c(-3,-2,-1), labels=c(3,2,1)) +
    scale_y_continuous(name="Logged normalized visits per 10K") +
    scale_color_manual(name='County Size', values=colors) +
    theme_classic()  
  
  p4 <- ggplot() +
    geom_line(data=state_dt, aes(x=time_id, y=delta, group=as.factor(location_id)), alpha=0.3) +
    geom_line(data=state_dt[size_IQR != 'mid'], aes(x=time_id, y=delta, group=as.factor(location_id), color=size_IQR), size=1) +
    geom_line(data=state_means, aes(x=time_id, y=delta), color='red', size=1.5) +
    scale_x_continuous(name="Weeks til mandate", breaks=c(-3,-2,-1), labels=c(3,2,1)) +
    scale_y_continuous(name="Change in logged normalized visits per 10K") +
    scale_color_manual(name='County Size', values=colors) +
    theme_classic()  
  
  # Lay out the plots into one figure
  combined <- p1 + p2 + p3 + p4 + 
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title=state_name)
  
  print(combined)
}
dev.off()
