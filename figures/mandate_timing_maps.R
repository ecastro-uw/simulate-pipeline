# EDA: second imposition of bars and restaurants

# packages
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(stringr)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# dirs
input_root  <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/"
output_root <- "/ihme/scratch/users/ems2285/thesis/aim_3/"
shape_file_path <- "/snfs1/WORK/11_geospatial/admin_shapefiles/2024_07_29/lbd_standard_admin_2.shp"

# Shared interval factor levels and colors (same for both mandate types)
interval_levels <- c('<2 weeks', '2-9 weeks', '10-19 weeks', '20-29 weeks', '30+ weeks')
COLS_interval   <- setNames(brewer.pal(5, "YlGnBu"), interval_levels)

# Load and subset shapefile once
map_A2_data <- st_read(shape_file_path)
map_A2_data <- map_A2_data[map_A2_data$ADM0_NAME == "United States", ]
map_contiguous <- map_A2_data[-which(map_A2_data$ADM1_NAME %in% c("Alaska", "Hawaii")), ]

# Shared theme for all map panels
map_theme <- theme(
  panel.background  = element_blank(),
  axis.text         = element_blank(),
  axis.ticks        = element_blank(),
  plot.title        = element_text(size = 10),
  legend.key.size   = unit(0.4, "cm"),
  legend.text       = element_text(size = 7),
  legend.title      = element_text(size = 8)
)

# Build plots for each mandate type
mandate_types <- c('restaurant', 'bar')
plot_pairs <- list()

for (mandate_type in mandate_types) {

  dt <- fread(paste0(input_root, 'second_', mandate_type, '_close.csv'))

  # Weeks between 2nd onset and 1st lift
  dt[, interval_wk  := floor(as.numeric(onset_date - prev_lift) / 7)]
  dt[, interval_cat := factor(
    fcase(
      interval_wk < 2,  '<2 weeks',
      interval_wk < 10, '2-9 weeks',
      interval_wk < 20, '10-19 weeks',
      interval_wk < 30, '20-29 weeks',
      default =         '30+ weeks'
    ),
    levels = interval_levels
  )]

  # Onset date category (sorted chronologically)
  onset_levels <- unique(format(sort(dt$onset_date), '%b %Y'))
  dt[, onset_date_cat := factor(format(onset_date, '%b %Y'), levels = onset_levels)]
  COLS_onset <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(onset_levels))

  # Merge data onto shapefile
  map_dt <- merge(
    map_contiguous,
    dt[, .(location_id, onset_date_cat, interval_cat)],
    by.x = 'loc_id', by.y = 'location_id', all.x = TRUE
  )

  type_label <- str_to_sentence(mandate_type)

  # Panel: timing of 2nd imposition
  onset_map <- ggplot(map_dt) +
    geom_sf(aes(geometry = geometry, fill = onset_date_cat), color = NA) +
    scale_fill_manual(
      name     = paste(type_label, "onset date"),
      values   = COLS_onset,
      na.value = "grey85"
    ) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE,
                               title.position = 'top', title.hjust = 0.5)) +
    labs(title = paste(type_label, "\u2014 Timing of Second Mandate")) +
    map_theme

  # Panel: interval between 1st lift and 2nd onset
  interval_map <- ggplot(map_dt) +
    geom_sf(aes(geometry = geometry, fill = interval_cat), color = NA) +
    scale_fill_manual(
      name     = "Weeks since first mandate",
      values   = COLS_interval,
      na.value = "grey85",
      drop     = FALSE
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                               title.position = 'top', title.hjust = 0.5)) +
    labs(title = paste(type_label, "\u2014 Interval Since First Mandate")) +
    map_theme

  plot_pairs[[mandate_type]] <- list(onset = onset_map, interval = interval_map)

  # Individual PDF (one per mandate type)
  pdf(paste0(output_root, "US_second_", mandate_type, "_mandates.pdf"), width = 9, height = 5)
  print(onset_map + interval_map + plot_layout(guides = 'collect') &
          theme(legend.position = 'bottom'))
  dev.off()
}

# Combined figure: onset legends under their own panels (A, B);
# shared interval legend centered below C and D.

# Row 1: onset maps — keep legend at bottom of each panel
p_A <- plot_pairs[['restaurant']]$onset    + theme(legend.position = 'bottom')
p_B <- plot_pairs[['bar']]$onset           + theme(legend.position = 'bottom')

# Row 2: interval maps — no legend (will be placed separately)
p_C <- plot_pairs[['restaurant']]$interval + theme(legend.position = 'none')
p_D <- plot_pairs[['bar']]$interval        + theme(legend.position = 'none')

# Extract interval legend as a grob, centered, to use as its own row
interval_leg <- cowplot::get_legend(
  plot_pairs[['restaurant']]$interval +
    theme(legend.position = 'bottom', legend.justification = 'center')
)

combined_plot <-
  (p_A | p_B) /
  (p_C | p_D) /
  wrap_elements(interval_leg) +
  plot_layout(heights = c(1, 1, 0.15)) +
  plot_annotation(
    title      = "Second Round of COVID-19 Bar & Restaurant Mandates by U.S. County",
    tag_levels = 'A'
  ) &
  theme(plot.tag = element_text(face = 'bold', size = 10))

pdf(paste0(output_root, "US_second_mandates.pdf"), width = 14, height = 10)
print(combined_plot)
dev.off()
