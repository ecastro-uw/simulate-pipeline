# Diagnostic Time Series Plots
#
# County-level time series of weekly visits to restaurants or bars in the weeks leading up to
# (and just after) the onset of a restaurant or bar closure, for every county in a state.
#
#   * mandate_type = 'restaurant' plots visits to restaurants around restaurant closures;
#     mandate_type = 'bar' plots visits to bars around bar closures.
#   * mandate_num  = 'first' or 'second' picks the closure instance.
#   * measure      = 'log_ratio' plots y = log((weekly visits + 0.5) / mean weekly baseline visits)
#     (the pipeline outcome); measure = 'count' plots raw weekly visit counts.
#   * The x-axis is calendar time. Weekly values are drawn at the midpoint of each 7-day week
#     (time_id t covers onset_date + 7t ... onset_date + 7t + 6), and the onset date (start of t = 0)
#     is marked with a dashed line, so pre-onset weeks fall to its left and t >= 0 to its right.
#   * Counties in the state that were dropped from the analysis are shown (where any data exist)
#     in a contrasting colour and listed, with the reason, in the plot caption.
#
# Weekly series are rebuilt from processed_safegraph_data.csv using the same rules as
# pipeline/1_prep_data.R (weekly sums of complete 7-day weeks, 35-day baseline from
# 2020-01-03 to 2020-02-06), so dropped counties and weeks outside the training window can be
# plotted too. check_against_pipeline() confirms this matches the pipeline's obs_context files.
#
# Layouts (large states such as TX or GA have 150+ counties):
#   * 'facet'   - one small panel per county, paginated (counties_per_page per page), counties
#                 ordered by onset date. Returns one plot per page.
#   * 'overlay' - all counties on one set of axes: faint lines, a point at each county's onset
#                 week, onset rug, and the state median of y by week relative to onset.
#                 Pass `highlight` (location_ids or county names) to emphasize and label a few.
#   * 'auto'    - (default) a single facet page if the state fits on one page; otherwise an
#                 overlay page followed by the paginated facet pages.

### Prep
# libraries
library(data.table)
library(ggplot2)
library(scales)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
version_id <- '20261013.02'   # only used by check_against_pipeline() and the output directory

# directories
root_dir  <- '/ihme/scratch/users/ems2285/thesis/outputs/outputs/'
covar_dir <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/'
config_dir  <- '/ihme/homes/ems2285/repos/simulate-pipeline/config_files/'
lookup_path <- paste0(config_dir, 'context_lookup_inv2_intersect_v3.csv')    # which contexts to analyze
locs_path   <- paste0(config_dir, 'locs_by_context_inv2_intersect_v3.csv')   # which counties are in each context
drop_path   <- paste0(config_dir, 'dropped_locations_inv2_v3.csv') # which counties are not included in the context and why
# The intersect contexts also exclude counties dropped in any onset-shift sensitivity analysis.
# These logs are used to explain exclusions that are not in drop_path.
sens_drop_paths <- c(
  'two weeks pre'  = paste0(config_dir, 'dropped_locations_inv2_TWO_WEEKS_PRE_v3.csv'),
  'one week pre'   = paste0(config_dir, 'dropped_locations_inv2_ONE_WEEK_PRE_v3.csv'),
  'one week post'  = paste0(config_dir, 'dropped_locations_inv2_ONE_WEEK_POST_v3.csv'),
  'two weeks post' = paste0(config_dir, 'dropped_locations_inv2_TWO_WEEKS_POST_v3.csv')
)
out_dir <- paste0(root_dir, version_id, '/county_timeseries/')

# location hierarchy info
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
states_dt <- hierarchy[level==2, .(parent_id = location_id, state_abbrev = gsub('US-','',local_id),
                                   state_name = location_name)]
counties_dt <- hierarchy[level==3, .(parent_id, location_id, location_name)]
locs_dt <- merge(counties_dt, states_dt, by='parent_id')


#### Load data
# Context definitions
context_info <- fread(lookup_path)[, .(context_id, mandate_num, mandate_type, state_abbrev, N)]
county_map   <- fread(locs_path)[, .(context_id, location_id)]

# Dropped locations: main analysis, then sensitivity analyses
dropped_locs <- fread(drop_path)[, .(location_id, event, reason)]
sens_dropped <- rbindlist(lapply(names(sens_drop_paths), function(nm) {
  fread(sens_drop_paths[[nm]])[, .(location_id, event, reason, analysis = nm)]
}))

# Mandate timing (first files have onset_date & lift; second files also have prev_lift)
onset_list <- list()
for (m_num in c('first', 'second')) {
  for (m_type in c('restaurant', 'bar')) {
    dt <- fread(paste0(covar_dir, m_num, '_', m_type, '_close.csv'))
    for (col in intersect(c('onset_date', 'lift', 'prev_lift'), names(dt))) set(dt, j = col, value = as.Date(dt[[col]]))
    if (!'prev_lift' %in% names(dt)) dt[, prev_lift := as.Date(NA)]
    onset_list[[paste(m_num, m_type, sep = '_')]] <- dt[, .(location_id, onset_date, lift, prev_lift)]
  }
}

# Daily SafeGraph visits (restaurants and bars only)
sg_dt <- fread(paste0(covar_dir, 'processed_safegraph_data.csv'),
               select = c('location_id', 'date', 'top_category', 'visit_count'))
sg_dt[, category := fifelse(top_category %like% 'Restaurants', 'restaurant',
                            fifelse(top_category %like% 'Drinking', 'bar', NA_character_))]
sg_dt <- sg_dt[!is.na(category), .(location_id, date = as.Date(date), category, v = visit_count)]
setkey(sg_dt, category, location_id, date)

# Baseline: mean weekly visits over the 35 days 2020-01-03 to 2020-02-06 (as in 1_prep_data.R).
# Counties without 35 complete days get mean_base = NA.
baseline_dt <- sg_dt[date >= as.Date('2020-01-03') & date <= as.Date('2020-02-06'),
                     .(n_days = sum(!is.na(v)), mean_base = sum(v, na.rm = TRUE) / 5),
                     by = .(category, location_id)]
baseline_dt[n_days < 35, mean_base := NA_real_]
baseline_dt[, n_days := NULL]


#### Helper functions
reason_labels <- c(
  no_mobility_data          = 'no SafeGraph data',
  missing_mobility          = 'missing SafeGraph data in study window',
  incomplete_baseline       = 'incomplete baseline data',
  short_interval            = 'too little time between mandates',
  missing_cases_deaths_data = 'missing COVID case/death data'
)
pretty_reason <- function(x) fifelse(x %in% names(reason_labels), reason_labels[x], gsub('_', ' ', x))
# shorter versions for facet strip labels
short_reason_labels <- c(
  no_mobility_data          = 'no SafeGraph data',
  missing_mobility          = 'missing SafeGraph data',
  incomplete_baseline       = 'incomplete baseline',
  short_interval            = 'short interval',
  missing_cases_deaths_data = 'missing case/death data'
)
short_reason <- function(x) fifelse(x %in% names(short_reason_labels), short_reason_labels[x], gsub('_', ' ', x))

# Build weekly series for a set of counties.
# Returns one row per location_id x time_id (time_id in -weeks_pre ... weeks_post - 1) with
# v (weekly visits; NA unless all 7 days are observed), y (log ratio), and week_mid (calendar date).
build_weekly_series <- function(onsets, mandate_type, weeks_pre, weeks_post) {
  if (nrow(onsets) == 0) return(data.table())
  day_offsets <- seq(-7 * weeks_pre, 7 * weeks_post - 1)
  daily <- onsets[, .(day = day_offsets), by = .(location_id, onset_date)]
  daily[, `:=`(date = onset_date + day, time_id = floor(day / 7))]
  daily <- merge(daily, sg_dt[category == mandate_type, .(location_id, date, v)],
                 by = c('location_id', 'date'), all.x = TRUE)
  weekly <- daily[, .(v = if (all(!is.na(v))) as.numeric(sum(v)) else NA_real_),
                  by = .(location_id, onset_date, time_id)]
  weekly <- merge(weekly, baseline_dt[category == mandate_type, .(location_id, mean_base)],
                  by = 'location_id', all.x = TRUE)
  weekly[, y := log((v + 0.5) / mean_base)]
  weekly[, week_mid := onset_date + 7 * time_id + 3]
  setorder(weekly, location_id, time_id)
  weekly[]
}

# Classify every county in the state for this mandate: included, dropped (with reason), or no mandate.
classify_counties <- function(state, mandate_type, mandate_num) {
  event_name <- paste(mandate_num, mandate_type, sep = '_')
  m_type <- mandate_type; m_num <- mandate_num
  ctx <- context_info[state_abbrev == state & mandate_type == m_type & mandate_num == m_num]
  included <- if (nrow(ctx) > 0) county_map[context_id %in% ctx$context_id, location_id] else integer()

  status <- locs_dt[state_abbrev == state, .(location_id, location_name)]
  onsets <- onset_list[[event_name]]
  status <- merge(status, onsets, by = 'location_id', all.x = TRUE)

  # reasons from the main analysis log (one row per county)
  main_r <- dropped_locs[event == event_name, .(reason_main = paste(unique(pretty_reason(reason)), collapse = '; '),
                                                 short_main  = paste(unique(short_reason(reason)), collapse = '; ')),
                         by = location_id]
  # reasons from the sensitivity analysis logs, for counties excluded only from the intersect
  sens_r <- sens_dropped[event == event_name,
                         .(reason_sens = paste0(paste(unique(pretty_reason(reason)), collapse = '; '),
                                                ' (onset shifted ', paste(unique(analysis), collapse = ', '), ')')),
                         by = location_id]
  status <- merge(status, main_r, by = 'location_id', all.x = TRUE)
  status <- merge(status, sens_r, by = 'location_id', all.x = TRUE)

  status[, status := fcase(
    location_id %in% included, 'included',
    is.na(onset_date),         'no mandate',
    default =                  'dropped'
  )]
  status[, reason := fcase(
    status == 'included',      NA_character_,
    status == 'no mandate',    paste('no', mandate_num, mandate_type, 'closure'),
    !is.na(reason_main),       reason_main,
    !is.na(reason_sens),       reason_sens,
    rep(nrow(ctx) == 0, .N),   'state not analyzed for this mandate',
    default =                  'excluded (reason not logged)'
  )]
  status[, reason_short := fcase(
    status != 'dropped',       NA_character_,
    !is.na(short_main),        short_main,
    !is.na(reason_sens),       'sensitivity analysis',
    rep(TRUE, .N),             reason
  )]
  status[, c('reason_main', 'reason_sens', 'short_main') := NULL]
  list(status = status[], context = ctx)
}

# Caption text summarizing dropped counties by reason.
dropped_caption <- function(status, max_names = 12, width = 150) {
  drp <- status[status == 'dropped']
  no_mand <- status[status == 'no mandate', .N]
  lines <- character()
  if (nrow(drp) > 0) {
    by_reason <- drp[order(location_name), .(n = .N, names = list(location_name)), by = reason][order(-n)]
    for (i in seq_len(nrow(by_reason))) {
      nm <- by_reason$names[[i]]
      nm_txt <- if (length(nm) > max_names) paste0(paste(nm[1:max_names], collapse = ', '), ', +', length(nm) - max_names, ' more')
                else paste(nm, collapse = ', ')
      lines <- c(lines, strwrap(paste0(by_reason$reason[i], ' (', by_reason$n[i], '): ', nm_txt),
                                width = width, exdent = 4))
    }
    lines <- c(paste0('Dropped counties (', nrow(drp), '):'), lines)
  } else {
    lines <- 'No counties dropped.'
  }
  if (no_mand > 0) lines <- c(lines, paste0(no_mand, ' count', ifelse(no_mand == 1, 'y', 'ies'),
                                            ' in the state did not impose this closure (not shown).'))
  paste(lines, collapse = '\n')
}

# Resolve `highlight` (location_ids or county names) to location_ids
resolve_highlight <- function(highlight, status) {
  if (is.null(highlight)) return(integer())
  if (is.numeric(highlight)) return(intersect(as.integer(highlight), status$location_id))
  status[tolower(location_name) %in% tolower(highlight) |
           tolower(gsub(' County$| Parish$| Borough$', '', location_name)) %in% tolower(highlight), location_id]
}


#### Plot functions
status_colours <- c(included = '#2C6E9B', dropped = '#D55E00')

facet_page <- function(dt, events, shade, status, y_col, y_lab, free_y, free_x, n_col, title, subtitle, caption) {
  facet_scales <- if (free_x && free_y) 'free' else if (free_x) 'free_x' else if (free_y) 'free_y' else 'fixed'
  ggplot(dt, aes(x = week_mid, y = .data[[y_col]], colour = status)) +
    geom_rect(data = shade, inherit.aes = FALSE,
              aes(xmin = onset_date, xmax = x_max, ymin = -Inf, ymax = Inf), fill = 'grey92') +
    geom_vline(data = events, aes(xintercept = date, linetype = event), colour = 'grey30', linewidth = 0.4) +
    geom_line(linewidth = 0.5, na.rm = TRUE) +
    geom_point(size = 0.9, na.rm = TRUE) +
    facet_wrap(~panel, ncol = n_col, scales = facet_scales) +
    scale_colour_manual(values = status_colours, name = NULL, drop = FALSE) +
    scale_linetype_manual(values = c('Mandate onset' = 'dashed', 'Previous mandate lifted' = 'dotted',
                                     'Mandate lifted' = 'dotdash'), name = NULL) +
    scale_x_date(name = NULL, date_labels = '%b %d', breaks = breaks_pretty(n = 4)) +
    scale_y_continuous(name = y_lab, labels = if (y_col == 'v') label_comma() else waiver()) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_bw(base_size = 9) +
    theme(legend.position = 'top', strip.text = element_text(size = 7),
          plot.caption = element_text(hjust = 0, size = 7),
          axis.text.x = element_text(size = 6))
}

overlay_plot <- function(dt, status, y_col, y_lab, highlight_ids, show_median, title, subtitle, caption) {
  onsets_plot <- unique(dt[, .(location_id, onset_date, status)])
  onset_pts <- dt[time_id == 0 & !is.na(get(y_col))]
  # state median of included counties by week relative to onset, drawn at the median calendar position
  med <- dt[status == 'included' & !is.na(get(y_col)),
            .(med = median(get(y_col)), x = median(week_mid), n = .N), by = time_id]
  hl <- dt[location_id %in% highlight_ids]
  hl_lab <- hl[!is.na(get(y_col)), .SD[which.max(time_id)], by = location_id]

  # a vertical line for each onset date when there are only a few distinct dates (e.g. statewide orders)
  uniq_onsets <- unique(onsets_plot[status == 'included', onset_date])

  p <- ggplot(dt, aes(x = week_mid, y = .data[[y_col]], group = location_id, colour = status))
  if (length(uniq_onsets) <= 3) {
    p <- p + geom_vline(xintercept = uniq_onsets, linetype = 'dashed', colour = 'grey30', linewidth = 0.4)
  }
  p <- p +
    geom_line(alpha = 0.35, linewidth = 0.3, na.rm = TRUE) +
    geom_point(data = onset_pts, size = 0.8, alpha = 0.6) +
    geom_rug(data = onsets_plot, aes(x = onset_date), inherit.aes = FALSE, sides = 'b',
             colour = 'grey20', alpha = 0.5, length = unit(0.02, 'npc')) +
    scale_colour_manual(values = status_colours, name = NULL, drop = FALSE) +
    scale_x_date(name = NULL, date_labels = '%b %d', breaks = breaks_pretty(n = 8),
                 expand = expansion(mult = c(0.03, ifelse(nrow(hl) > 0, 0.15, 0.03)))) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_bw(base_size = 10) +
    theme(legend.position = 'top', plot.caption = element_text(hjust = 0, size = 7))
  if (show_median && nrow(med) > 0) {
    p <- p + geom_line(data = med, aes(x = x, y = med), inherit.aes = FALSE, colour = 'black', linewidth = 1)
  }
  if (nrow(hl) > 0) {
    p <- p + geom_line(data = hl, linewidth = 1, na.rm = TRUE) +
      geom_point(data = hl, size = 1.3, na.rm = TRUE) +
      geom_text(data = hl_lab, aes(label = location_name), hjust = -0.1, size = 2.8, show.legend = FALSE)
  }
  if (y_col == 'v') {
    # visit counts span orders of magnitude across counties; pseudo-log keeps zeros on the axis
    p <- p + scale_y_continuous(name = paste(y_lab, '(pseudo-log scale)'), trans = pseudo_log_trans(base = 10),
                                labels = label_comma(), breaks = c(0, 10, 100, 1e3, 1e4, 1e5, 1e6))
  } else {
    p <- p + scale_y_continuous(name = y_lab)
  }
  p
}

# Main function
#   state             two-letter state abbreviation, e.g. 'WA'
#   mandate_type      'restaurant' or 'bar' (visits plotted match the mandate type)
#   mandate_num       'first' or 'second'
#   measure           'log_ratio' (y = log((visits + 0.5) / mean baseline)) or 'count' (weekly visits)
#   layout            'auto', 'facet', or 'overlay' (see header)
#   weeks_pre         weeks before onset to show (default 8 = default_train_wks)
#   weeks_post        weeks from onset onward to show (t = 0 ... weeks_post - 1)
#   include_dropped   plot dropped counties that have data (they are always listed in the caption)
#   counties_per_page panels per facet page; n_col columns per page
#   free_y            per-panel y-axes in facet layout (default: TRUE for counts, FALSE for log ratio)
#   free_x            per-panel x-axes in facet layout (default: TRUE when onset dates span > 3 weeks,
#                     so each panel shows its own window instead of a squashed shared one)
#   highlight         location_ids or county names to emphasize in the overlay layout
#   auto_threshold    'auto' switches to overlay + paginated facets above this many counties
# Returns (invisibly) a list: plots (list of ggplots, one per page), data (weekly series), status
# (every county in the state with included/dropped/no mandate and reason).
plot_county_timeseries <- function(state, mandate_type = c('restaurant', 'bar'),
                                   mandate_num = c('first', 'second'),
                                   measure = c('log_ratio', 'count'),
                                   layout = c('auto', 'facet', 'overlay'),
                                   weeks_pre = 8, weeks_post = 2,
                                   include_dropped = TRUE,
                                   counties_per_page = 30, n_col = 6,
                                   free_y = NULL, free_x = NULL, highlight = NULL,
                                   auto_threshold = counties_per_page,
                                   print_plots = TRUE) {
  mandate_type <- match.arg(mandate_type)
  mandate_num  <- match.arg(mandate_num)
  measure      <- match.arg(measure)
  layout       <- match.arg(layout)
  if (is.null(free_y)) free_y <- measure == 'count'

  cls <- classify_counties(state, mandate_type, mandate_num)
  status <- cls$status
  show_status <- if (include_dropped) c('included', 'dropped') else 'included'
  to_plot <- status[status %in% show_status]

  dt <- build_weekly_series(to_plot[, .(location_id, onset_date)], mandate_type, weeks_pre, weeks_post)
  y_col <- ifelse(measure == 'count', 'v', 'y')
  visit_word <- ifelse(mandate_type == 'restaurant', 'restaurants', 'bars')
  y_lab <- ifelse(measure == 'count', paste('Weekly visits to', visit_word),
                  'log((weekly visits + 0.5) / mean weekly baseline visits)')

  # counties with no plottable data (e.g. no SafeGraph data, or no baseline for the log ratio)
  if (nrow(dt) > 0) {
    has_data <- dt[!is.na(get(y_col)), unique(location_id)]
    dt <- dt[location_id %in% has_data]
  } else has_data <- integer()
  to_plot[, plotted := location_id %in% has_data]

  dt <- merge(dt, to_plot[, .(location_id, location_name, status, reason)], by = 'location_id')
  dt[, status := factor(status, levels = c('included', 'dropped'))]

  # titles
  state_name <- states_dt[state_abbrev == state, state_name]
  title <- paste0(state_name, ': ', mandate_num, ' ', mandate_type, ' closure')
  n_inc <- status[status == 'included', .N]
  n_drp <- status[status == 'dropped', .N]
  n_not_plotted <- to_plot[plotted == FALSE, .N]
  onset_rng <- range(status[status %in% show_status, onset_date], na.rm = TRUE)
  subtitle <- paste0(
    ifelse(measure == 'count', 'Weekly visits to ', 'Log ratio of weekly visits to '), visit_word,
    ', ', weeks_pre, ' weeks before onset to ', weeks_post, ' week', ifelse(weeks_post == 1, '', 's'), ' after. ',
    n_inc, ' included, ', n_drp, ' dropped counties',
    if (n_not_plotted > 0) paste0(' (', n_not_plotted, ' without data to plot)') else '', '.',
    if (all(is.finite(onset_rng))) paste0('\nOnset dates: ', format(onset_rng[1], '%b %d'),
                                         if (onset_rng[1] != onset_rng[2]) paste0(' - ', format(onset_rng[2], '%b %d, %Y'))
                                         else format(onset_rng[1], ', %Y')) else '')
  caption <- dropped_caption(status)

  if (nrow(dt) == 0) {
    warning('No data to plot for ', state, ' ', mandate_num, ' ', mandate_type)
    return(invisible(list(plots = list(), data = dt, status = status)))
  }

  # panel order: included counties by onset date, then dropped counties
  ord <- merge(unique(dt[, .(location_id, location_name, status, onset_date)]),
               status[, .(location_id, reason_short)], by = 'location_id')
  setorder(ord, status, onset_date, location_name)
  ord[, panel := ifelse(status == 'dropped',
                        paste0(location_name, '\n(dropped: ', reason_short, ')'),
                        location_name)]
  ord[, panel := factor(panel, levels = unique(panel))]
  dt <- merge(dt, ord[, .(location_id, panel)], by = 'location_id')

  # mandate events per county (onset, plus previous lift / lift when they fall in the window)
  win <- dt[, .(x_min = min(week_mid) - 3, x_max = max(week_mid) + 3), by = .(location_id, panel)]
  ev <- merge(onset_list[[paste(mandate_num, mandate_type, sep = '_')]], win, by = 'location_id')
  events <- rbind(ev[, .(panel, date = onset_date, event = 'Mandate onset')],
                  ev[!is.na(prev_lift) & prev_lift >= x_min, .(panel, date = prev_lift, event = 'Previous mandate lifted')],
                  ev[!is.na(lift) & lift <= x_max, .(panel, date = lift, event = 'Mandate lifted')])

  n_counties <- uniqueN(dt$location_id)
  onsets_spread <- as.numeric(diff(range(ord$onset_date))) > 21
  if (is.null(free_x)) free_x <- onsets_spread
  plots <- list()
  if (layout == 'overlay' || (layout == 'auto' && n_counties > auto_threshold)) {
    plots[['overlay']] <- overlay_plot(
      dt, status, y_col, y_lab, resolve_highlight(highlight, status), !onsets_spread, title,
      paste0(subtitle, '\nPoints mark each county\'s onset week (t = 0); ticks on the x-axis mark onset dates',
             if (!onsets_spread) '; black line = median of included counties by week relative to onset.' else '.'),
      caption)
  }
  if (layout == 'facet' || layout == 'auto') {
    panels <- levels(droplevels(dt$panel))
    pages <- split(panels, ceiling(seq_along(panels) / counties_per_page))
    for (i in seq_along(pages)) {
      page_title <- if (length(pages) > 1) paste0(title, ' (page ', i, ' of ', length(pages), ')') else title
      plots[[paste0('facet_', i)]] <- facet_page(
        dt[panel %in% pages[[i]]], events[panel %in% pages[[i]]], ev[panel %in% pages[[i]]], status, y_col, y_lab,
        free_y, free_x, n_col, page_title, paste0(subtitle, '\nShaded region = weeks from onset onward.'), caption)
    }
  }

  if (print_plots) for (p in plots) print(p)
  invisible(list(plots = plots, data = dt[order(status, location_id, time_id)], status = status))
}

# Save one PDF per mandate category with every state (or a subset) for a given measure
save_timeseries_pdf <- function(mandate_type, mandate_num, measure = 'log_ratio',
                                states = sort(unique(context_info$state_abbrev)),
                                file = NULL, width = 14, height = 10, ...) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(file)) file <- paste0(out_dir, mandate_num, '_', mandate_type, '_', measure, '.pdf')
  pdf(file, width = width, height = height)
  on.exit(dev.off())
  for (st in states) {
    res <- tryCatch(plot_county_timeseries(st, mandate_type, mandate_num, measure, print_plots = TRUE, ...),
                    error = function(e) { message(st, ': ', conditionMessage(e)); NULL })
  }
  message('Saved ', file)
  invisible(file)
}

# Optional check: rebuilt log ratios should match the pipeline's obs_context files for included counties
check_against_pipeline <- function(context, weeks_pre = 8) {
  ctx <- context_info[context_id == context]
  obs <- fread(paste0(root_dir, version_id, '/batched_output/obs_context_', context, '.csv'))[
    !is.na(y), .(location_id, time_id, y_obs = y)]
  res <- plot_county_timeseries(ctx$state_abbrev, ctx$mandate_type, ctx$mandate_num, 'log_ratio',
                                weeks_pre = weeks_pre, weeks_post = 1, print_plots = FALSE)
  cmp <- merge(obs, res$data[, .(location_id, time_id, y)], by = c('location_id', 'time_id'), all.x = TRUE)
  cat('Context', context, ':', nrow(cmp), 'obs rows;', cmp[is.na(y), .N], 'not rebuilt; max |diff| =',
      cmp[, max(abs(y - y_obs), na.rm = TRUE)], '\n')
  invisible(cmp)
}


#### Examples
if (FALSE) {
  # one state, one mandate category
  plot_county_timeseries('WA', 'restaurant', 'first', measure = 'log_ratio')
  plot_county_timeseries('WA', 'bar', 'second', measure = 'count', layout = 'facet')

  # a large state: overlay with a few counties highlighted, then paginated facets
  tx <- plot_county_timeseries('TX', 'restaurant', 'first', highlight = c('Harris', 'Travis', 'Dallas'))
  tx$status[status != 'included']   # which counties were dropped and why

  # confirm the rebuilt series matches the pipeline's observations
  check_against_pipeline(context_info[state_abbrev == 'WA' & mandate_type == 'restaurant' & mandate_num == 'first', context_id])

  # all states, all four mandate categories
  for (m_num in c('first', 'second')) for (m_type in c('restaurant', 'bar')) {
    save_timeseries_pdf(m_type, m_num, measure = 'log_ratio')
    save_timeseries_pdf(m_type, m_num, measure = 'count')
  }
}
