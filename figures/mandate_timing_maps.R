# EDA: second imposition of bars and restaurants

# packages
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(stringr)

# Which country are you mapping?
country <- 'United States' #United States or Brazil

# Define dirs
root <- "/ihme/scratch/users/ems2285/thesis/aim_3"
if (country=='United States'){
  input_root  <- file.path(root, "processed_data/USA_counties/")
  shape_file_path <- "/snfs1/WORK/11_geospatial/admin_shapefiles/2024_07_29/lbd_standard_admin_2.shp"
} else if (country=='Brazil'){
  input_root  <- file.path(root, "processed_data/Brazil_states/")
  shape_file_path <- "/snfs1/WORK/11_geospatial/admin_shapefiles/2024_07_29/lbd_standard_admin_1.shp"  
}


# Define shared interval factor levels and colors (same for both mandate types)
interval_levels <- c('<2 weeks', '2-9 weeks', '10-19 weeks', '20-29 weeks', '30+ weeks')
COLS_interval   <- setNames(brewer.pal(5, "YlGnBu"), interval_levels)

# Load and subset shape file once
map_data <- st_read(shape_file_path)
map_data <- map_data[map_data$ADM0_NAME == country, ]
if(country=='United States'){
  map_data <- map_data[-which(map_data$ADM1_NAME %in% c("Alaska", "Hawaii")), ]
}


# Shared theme for all map panels
map_theme <- theme(
  panel.background  = element_blank(),
  axis.text         = element_blank(),
  axis.ticks        = element_blank(),
  plot.title        = element_text(size = 12),
  legend.key.size   = unit(0.4, "cm"),
  legend.text       = element_text(size = 11),
  legend.title      = element_text(size = 11) #,
  #plot.margin       = margin(0,0,0,0)
)

# Define plot title
if(country=='United States'){
  main_title <- "United States: Second Round of COVID-19 Bar & Restaurant Mandates by County"
} else if (country=='Brazil'){
  main_title <- "Brazil: Second Round of COVID-19 Bar & Restaurant Mandates by State"
}

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

  # Merge data onto shape file
  map_dt <- merge(
    map_data,
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
    labs(title = paste0(type_label, "s: Second Mandate Onset")) +
    map_theme
  

  # Panel: interval between 1st lift and 2nd onset
  legend_nrow <- ifelse(country=='United States', 1, 2)
  interval_map <- ggplot(map_dt) +
    geom_sf(aes(geometry = geometry, fill = interval_cat), color = NA) +
    scale_fill_manual(
      name     = "Weeks since first mandate lifted",
      values   = COLS_interval,
      na.value = "grey85",
      drop     = FALSE
    ) +
    guides(fill = guide_legend(nrow = legend_nrow, byrow = TRUE,
                               title.position = 'top', title.hjust = 0.5)) +
    labs(title = paste0(type_label, "s: Interval Between 1st and 2nd Mandate")) +
    map_theme
  
  if(country=='Brazil'){
    onset_map <- onset_map +
      coord_sf(xlim=c(-74, -28),
               ylim=c(-34, 6),
               expand=FALSE)
    
    interval_map <- interval_map +
      coord_sf(xlim=c(-74, -28),
               ylim=c(-34, 6),
               expand=FALSE)
  }

  plot_pairs[[mandate_type]] <- list(onset = onset_map, interval = interval_map)

  # Individual PDF (one per mandate type)
  #pdf(paste0(root, "/", country,"_second_", mandate_type, "_mandates.pdf"), width = 9, height = 5)
  #print(onset_map + interval_map + plot_layout(guides = 'collect') &
  #        theme(legend.position = 'bottom'))
  #dev.off()
}

# Combined figure: onset legends under their own panels (A, B)
# shared interval legend centered below C and D

# Row 1: onset maps 
p_A <- plot_pairs[['restaurant']]$onset    + theme(legend.position = 'bottom')
p_B <- plot_pairs[['bar']]$onset           + theme(legend.position = 'bottom')

# Row 2: interval maps 
p_C <- plot_pairs[['restaurant']]$interval + theme(legend.position = 'bottom')
p_D <- plot_pairs[['bar']]$interval        + theme(legend.position = 'bottom')

# Combined plot
combined_plot <-
  (p_A | p_B) /
  (p_C | p_D) /
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    title      = main_title,
    tag_levels = 'A'
  ) &
  theme(plot.tag = element_text(face = 'bold', size=10),
        plot.margin = margin(0,0,0,0))
#        plot.margin = margin(1,25,1,25)) #brazil png needed more spacing

if(country=='Brazil'){
  pdf_width <- 10
} else {
  pdf_width <- 14
}

pdf(paste0(root, "/", country, "_second_mandates.pdf"), width = pdf_width, height = 10)
print(combined_plot)
dev.off()

ggsave(paste0(root, "/", country, "_second_mandates.png"), plot = combined_plot, width = 14, height = 12, dpi=300)
