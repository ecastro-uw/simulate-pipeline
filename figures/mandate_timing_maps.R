# EDA: second imposition of bars and restaurants

# packages
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# dirs
input_root <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/"
shape_file_path <- "/snfs1/WORK/11_geospatial/admin_shapefiles/2024_07_29/lbd_standard_admin_2.shp"

# args
mandate_type <- 'bar'

# Load the data
dt <- fread(paste0(input_root,'second_',mandate_type,'_close.csv'))

# Calculate the number of weeks between 2nd onset and 1st lift
dt[, interval := onset_date - prev_lift]
dt[, interval_wk := floor((onset_date - prev_lift)/7)]
dt[, interval_cat := ifelse(interval_wk<2, '<2 weeks',
                            ifelse(interval_wk<10, '2-9 weeks',
                                   ifelse(interval_wk<20, '10-19 weeks',
                                          ifelse(interval_wk<30, '20-29 weeks', '30+ weeks'))))]
dt[, interval_cat := factor(interval_cat, levels=unique(dt[order(interval), interval_cat]))]

# Format onset date
dt[, onset_date_cat := factor(format(dt$onset_date, '%b %Y'),
                              levels = unique(format(sort(dt$onset_date), '%b %Y')))]


# Prep the map
map_A2_data <- st_read(shape_file_path)
map_A2_data <- map_A2_data[map_A2_data$ADM0_NAME == "United States",]
map_A2_data_contiguous <- map_A2_data[-which(map_A2_data$ADM1_NAME %in% c("Alaska", "Hawaii")),]
# Add data 
map_A2_data_contiguous <- merge(map_A2_data_contiguous, dt[,.(location_id, onset_date_cat, interval_cat)],
                                by.x='loc_id', by.y='location_id', all.x=T)

# Define colors
COLS <- colorRampPalette(brewer.pal(9,"YlGnBu"))(length(unique(dt$onset_date_cat)))
COLS2 <- brewer.pal(5,"YlGnBu")

# Plot onset date of 2nd imposition
onset_map_bar <- ggplot(data = map_A2_data_contiguous) +
  geom_sf(aes(geometry = geometry, fill = onset_date_cat)) + 
  scale_fill_manual(name = "Onset Date", values=COLS) +
  guides(fill = guide_legend(nrow = 2, byrow=T, position='bottom', title.position='top', title.hjust=0.5)) +
  ggtitle(paste("Timing of Second", str_to_sentence(mandate_type), "Mandate")) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())

# Plot interval between 1st and 2nd imposition
interval_map_bar <- ggplot(data = map_A2_data_contiguous) +
  geom_sf(aes(geometry = geometry, fill = interval_cat)) + 
  scale_fill_manual(name = "Interval", values=COLS2) +
  guides(fill = guide_legend(nrow = 1, byrow=T, position='bottom', title.position='top', title.hjust=0.5)) +
  ggtitle(paste("Interval Between First and Second", str_to_sentence(mandate_type), "Mandate")) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())

pdf(paste0("/ihme/scratch/users/ems2285/thesis/aim_3/US_second_", mandate_type,"_mandates.pdf"))
onset_map_bar
interval_map_bar
dev.off()


# Make a grid of plots
pdf(paste0("/ihme/scratch/users/ems2285/thesis/aim_3/US_second_mandates.pdf"), width=9)
onset_map + interval_map + onset_map_bar + interval_map_bar
dev.off()
