# Compare context-level characteristics across Test 1 / Test 2 verdict groups
#
# (1) For each test, convert the result to a binary 1/0 and combine the
#     sensitivity weeks (-2, -1, 0, 1, 2) into one wide table (as in deep_dive.R)
# (2) Categorize every context (row of test*_dt) into one of four groups:
#       A: only week 0 = 1, all other weeks = 0
#       B: all weeks = 0 and observed mobility has LOW variability
#       C: all weeks = 0 and observed mobility has HIGH variability
#       D: other
# (3) Test whether the four groups differ with respect to:
#       (a) political leaning      - % voted Republican (2020 presidential)
#       (b) political heterogeneity - SD of county-level % Republican
#       (c) baseline visit volume   - median restaurant/bar visits per 10K pop during 2019
#       (d) baseline visit heterogeneity - CV of county-level baseline visit volume
#       (e) cumulative COVID cases per 10K pop at mandate onset
#       (f) population
#     separately for each mandate category (first/second restaurant/bar closures).
# Contexts with fewer than min_counties counties are dropped.

## SET UP
# libraries
library(data.table)
library(ggplot2)
library(patchwork)

# args
Scenario <- 'C_new'
alpha    <- 0.05
min_counties <- 10   # drop contexts with fewer counties than this

# dirs
versions_path <- '/ihme/scratch/users/ems2285/thesis/inputs/sensitivity_version_ids.csv'
root_dir  <- '/ihme/scratch/users/ems2285/thesis/outputs/outputs/'
covar_dir <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/'

## LOADING
versions_dt <- fread(versions_path, colClasses = 'character')[scenario==Scenario]
list_of_cols <- c('context_id', 'mandate_num', 'mandate_type', 'state_abbrev', 'N')


### 1. CONVERT RESULTS TO BINARY AND COMBINE WEEKS

# define a function for processing one week (both tests)
combine_results <- function(row){
  # resolve versions
  week <- versions_dt[row, week]
  version <- versions_dt[row, version]
  input_path <- file.path(root_dir, version)

  # load results from test 1
  p_val <- fread(paste0(input_path,'/effect_size_summary.csv'))[, .SD, .SDcols = c(list_of_cols, 'binom_p')]
  p_val[, (week) := ifelse(binom_p < 0.05, 1, 0)]
  p_val$binom_p <- NULL

  # load results from test 2
  rr <- fread(paste0(input_path,'/effect_size_meta.csv'))[, .SD, .SDcols = c(list_of_cols, 'direction')]
  rr[, (week) := ifelse(direction=='decrease', 1, 0)]
  rr$direction <- NULL

  list(p_val = p_val, rr = rr)
}

# Build one wide table per test, one column per week
test1_dt <- NULL
test2_dt <- NULL

for (row in seq_len(nrow(versions_dt))) {
  result <- combine_results(row)
  if (is.null(test1_dt)) {
    test1_dt <- result$p_val
    test2_dt <- result$rr
  } else {
    test1_dt <- merge(test1_dt, result$p_val, by = list_of_cols, all = TRUE)
    test2_dt <- merge(test2_dt, result$rr, by = list_of_cols, all = TRUE)
  }
}

week_cols <- setdiff(names(test1_dt), list_of_cols)
week0_col <- "0"

# The week 0 version holds the unshifted mandate onset; use it for observed data
week0_vers <- versions_dt[week==week0_col, version]


### 2. CATEGORIZE CONTEXTS

## 2(a) Variability of the observed mobility data
# For each county, variability = SD of the week-to-week change in y (log normalized
# visits) over the pre-mandate weeks (time_id < 0). Differencing removes the
# pre-mandate trend so that only the noise is captured. The context-level value is
# the median across the context's counties.
calc_obs_variability <- function(context){
  obs_dt <- fread(paste0(root_dir, week0_vers, '/batched_output/obs_context_', context, '.csv'))[
    time_id < 0 & !is.na(y), .(location_id, time_id, y)]
  setorder(obs_dt, location_id, time_id)
  county_var <- obs_dt[, .(sd_diff = sd(diff(y))), by = location_id]
  list(
    var_dt = data.table(context_id = context, obs_var = median(county_var$sd_diff, na.rm = TRUE)),
    county_dt = data.table(context_id = context, location_id = unique(obs_dt$location_id))
  )
}

all_contexts <- sort(unique(c(test1_dt$context_id, test2_dt$context_id)))
obs_results  <- lapply(all_contexts, calc_obs_variability)
obs_var_dt   <- rbindlist(lapply(obs_results, `[[`, 'var_dt'))
context_counties <- rbindlist(lapply(obs_results, `[[`, 'county_dt'))

# Drop contexts with fewer than min_counties counties (counted from the obs_context file)
n_counties  <- context_counties[, .(n_counties = .N), by = context_id]
small_ctx   <- n_counties[n_counties < min_counties, context_id]
small_ctx   <- c(small_ctx, setdiff(all_contexts, n_counties$context_id))
message('Dropping ', length(small_ctx), ' of ', length(all_contexts),
        ' contexts with fewer than ', min_counties, ' counties')
test1_dt <- test1_dt[! context_id %in% small_ctx]
test2_dt <- test2_dt[! context_id %in% small_ctx]
obs_var_dt <- obs_var_dt[! context_id %in% small_ctx]
context_counties <- context_counties[! context_id %in% small_ctx]

# Low/high split: median variability across all retained contexts, so that a given
# context receives the same low/high label under both tests
var_threshold <- median(obs_var_dt$obs_var, na.rm = TRUE)
obs_var_dt[, var_cat := ifelse(obs_var <= var_threshold, 'low', 'high')]
message('Mobility variability threshold (median SD of weekly change in y): ', round(var_threshold, 4))

## 2(b) Assign groups
group_labels <- c(A = 'A: week 0 only',
                  B = 'B: all 0, low var',
                  C = 'C: all 0, high var',
                  D = 'D: other')

assign_groups <- function(dt){
  dt <- merge(copy(dt), obs_var_dt, by = 'context_id', all.x = TRUE)
  dt[, n_pos := rowSums(.SD), .SDcols = week_cols]
  dt[, group := fcase(
    get(week0_col) == 1 & n_pos == 1,       'A',
    n_pos == 0 & var_cat == 'low',           'B',
    n_pos == 0 & var_cat == 'high',          'C',
    n_pos > 0,                               'D'
  )]
  # contexts missing a result for any week (or missing obs data, if all weeks = 0) are left unassigned
  n_missing <- dt[is.na(group), .N]
  if (n_missing > 0) message(n_missing, ' context(s) could not be assigned a group (missing week results or obs data)')
  dt[, group := factor(group, levels = names(group_labels), labels = group_labels)]
  dt
}

test1_dt <- assign_groups(test1_dt)
test2_dt <- assign_groups(test2_dt)

print(dcast(test1_dt, mandate_num + mandate_type ~ group, fun.aggregate = length, value.var = 'context_id'))
print(dcast(test2_dt, mandate_num + mandate_type ~ group, fun.aggregate = length, value.var = 'context_id'))


### 3. CONTEXT-LEVEL COVARIATES
# Every covariate is computed over the counties included in the context's
# obs_context file (week 0 version)

# Context definitions (one row per context)
context_info <- unique(rbind(test1_dt[, .(context_id, mandate_num, mandate_type, state_abbrev)],
                             test2_dt[, .(context_id, mandate_num, mandate_type, state_abbrev)]))
county_map <- merge(context_counties, context_info, by = 'context_id')

# Population
pop_dt <- fread(paste0(covar_dir, 'population.csv'))[, .(location_id, pop)]
county_map <- merge(county_map, pop_dt, by = 'location_id', all.x = TRUE)

## (f) Population
covars_dt <- county_map[, .(population = as.numeric(sum(pop, na.rm = TRUE))), by = context_id]

## (a), (b) Political leaning and heterogeneity
elect_dt <- fread(paste0(covar_dir, 'election_results.csv'))[party=='REPUBLICAN', .(location_id, votes, total_votes)]
elect_dt <- merge(county_map[, .(context_id, location_id)], elect_dt, by = 'location_id')
pol_dt <- elect_dt[, .(pct_rep = sum(votes) / sum(total_votes),       # pooled across counties
                       pct_rep_sd = sd(votes / total_votes)),         # spread of county-level shares
                   by = context_id]
covars_dt <- merge(covars_dt, pol_dt, by = 'context_id', all.x = TRUE)

## (c), (d) Baseline visit volume and heterogeneity (2019)
sg_dt <- fread(paste0(covar_dir, 'processed_safegraph_data.csv'))[
  date >= '2019-01-01' & date <= '2019-12-31', .(location_id, date, top_category, visit_count)]
sg_dt[, mandate_type := fcase(top_category %like% 'Restaurant', 'restaurant',
                              top_category %like% 'Drinking',   'bar')]
sg_dt <- sg_dt[!is.na(mandate_type) & !is.na(visit_count),
               .(visit_count = sum(visit_count)), by = .(location_id, date, mandate_type)]
sg_dt <- merge(county_map[, .(context_id, location_id, mandate_type, pop)], sg_dt,
               by = c('location_id', 'mandate_type'))

# Context level: pool visits and population across counties each day, then take the 2019 median
visits_ctx <- sg_dt[, .(visits_per_10k = sum(visit_count) / sum(pop) * 10000), by = .(context_id, date)]
visits_ctx <- visits_ctx[, .(base_visits = median(visits_per_10k)), by = context_id]

# Heterogeneity: coefficient of variation of county-level 2019 medians
# (CV rather than SD, so that heterogeneity is not just a function of volume)
visits_cty <- sg_dt[, .(visits_per_10k = median(visit_count / pop * 10000)), by = .(context_id, location_id)]
visits_cty <- visits_cty[, .(base_visits_cv = sd(visits_per_10k) / mean(visits_per_10k)), by = context_id]

covars_dt <- merge(covars_dt, visits_ctx, by = 'context_id', all.x = TRUE)
covars_dt <- merge(covars_dt, visits_cty, by = 'context_id', all.x = TRUE)

## (e) Cumulative COVID cases per 10K at mandate onset
onset_dt <- rbindlist(lapply(c('first', 'second'), function(m_num){
  rbindlist(lapply(c('restaurant', 'bar'), function(m_type){
    fread(paste0(covar_dir, m_num, '_', m_type, '_close.csv'))[
      , .(location_id, onset_date, mandate_num = m_num, mandate_type = m_type)]
  }))
}))
cases_dt <- fread(paste0(covar_dir, 'covid_cases_deaths.csv'))[, .(location_id, date, cuml_cases)]
cases_ctx <- merge(county_map[, .(context_id, location_id, mandate_num, mandate_type, pop)], onset_dt,
                   by = c('location_id', 'mandate_num', 'mandate_type'))
cases_ctx <- merge(cases_ctx, cases_dt, by.x = c('location_id', 'onset_date'),
                   by.y = c('location_id', 'date'), all.x = TRUE)
# no row on the onset date means no cases had been reported yet
cases_ctx[is.na(cuml_cases), cuml_cases := 0]
cases_ctx <- cases_ctx[, .(cases_per_10k = sum(cuml_cases) / sum(pop) * 10000), by = context_id]
covars_dt <- merge(covars_dt, cases_ctx, by = 'context_id', all.x = TRUE)

# Variable labels; skewed variables are log-transformed for the ANOVA
covar_info <- data.table(
  var   = c('pct_rep', 'pct_rep_sd', 'base_visits', 'base_visits_cv', 'cases_per_10k', 'population'),
  label = c('% voted Republican (2020)', 'Political heterogeneity (SD of county % Rep)',
            'Baseline visits per 10K (2019 median)', 'Baseline visit heterogeneity (CV across counties)',
            'Cumulative cases per 10K at onset', 'Population'),
  log_for_anova = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
)


### 4. STATISTICAL TESTS
# Separately for each test, mandate category and covariate:
#  - one-way ANOVA across the four groups (log scale for skewed variables)
#  - Kruskal-Wallis test (non-parametric; robust to small, skewed groups)
#  - pairwise Wilcoxon tests (Holm-adjusted) when Kruskal-Wallis p < alpha

mandate_cats <- CJ(mandate_num = c('first', 'second'), mandate_type = c('restaurant', 'bar'))
mandate_cats[, category := paste(mandate_num, mandate_type)]

compare_groups <- function(test_dt, test_name, m_num, m_type){
  dt <- merge(test_dt[!is.na(group) & mandate_num == m_num & mandate_type == m_type,
                      .(context_id, mandate_num, mandate_type, group)],
              covars_dt, by = 'context_id')
  category <- paste(m_num, m_type)

  summary_list  <- list()
  pairwise_list <- list()

  for (i in seq_len(nrow(covar_info))) {
    v <- covar_info[i, var]
    sub <- dt[!is.na(get(v)) & is.finite(get(v))]
    sub[, y := if (covar_info[i, log_for_anova]) log(get(v) + 1) else get(v)]
    # only keep groups with at least 2 contexts
    keep <- sub[, .N, by = group][N >= 2, group]
    sub <- sub[group %in% keep]
    sub[, group := droplevels(group)]

    anova_p <- kw_p <- NA_real_
    if (uniqueN(sub$group) >= 2) {
      anova_p <- summary(aov(y ~ group, data = sub))[[1]][['Pr(>F)']][1]
      kw_p <- kruskal.test(sub[[v]], sub$group)$p.value

      if (!is.na(kw_p) && kw_p < alpha) {
        pw <- pairwise.wilcox.test(sub[[v]], sub$group, p.adjust.method = 'holm', exact = FALSE)$p.value
        pw <- as.data.table(as.table(pw))[!is.na(N)]
        setnames(pw, c('group_1', 'group_2', 'p_holm'))
        pairwise_list[[v]] <- cbind(test = test_name, category = category, variable = v, pw)
      }
    }

    # group medians (original scale) for interpretation
    meds <- dt[!is.na(get(v)), .(median = as.numeric(median(get(v)))), by = group]
    meds <- if (nrow(meds) > 0) dcast(meds, . ~ group, value.var = 'median')[, -1] else data.table()
    if (ncol(meds) > 0) setnames(meds, paste0('median_', substr(names(meds), 1, 1)))

    summary_list[[v]] <- cbind(
      data.table(test = test_name, category = category, variable = v, label = covar_info[i, label],
                 n_contexts = nrow(sub), n_groups = uniqueN(sub$group),
                 anova_p = anova_p, kruskal_p = kw_p),
      meds)
  }

  list(test = test_name, category = category, data = dt,
       summary = rbindlist(summary_list, fill = TRUE),
       pairwise = rbindlist(pairwise_list, fill = TRUE))
}

results <- list()
for (k in seq_len(nrow(mandate_cats))) {
  m_num  <- mandate_cats[k, mandate_num]
  m_type <- mandate_cats[k, mandate_type]
  results[[paste('Test 1', m_num, m_type)]] <- compare_groups(test1_dt, 'Test 1', m_num, m_type)
  results[[paste('Test 2', m_num, m_type)]] <- compare_groups(test2_dt, 'Test 2', m_num, m_type)
}

summary_dt  <- rbindlist(lapply(results, `[[`, 'summary'), fill = TRUE)
pairwise_dt <- rbindlist(lapply(results, `[[`, 'pairwise'), fill = TRUE)
setcolorder(summary_dt, intersect(c('test', 'category', 'variable', 'label', 'n_contexts', 'n_groups',
                                    'anova_p', 'kruskal_p', paste0('median_', names(group_labels))),
                                  names(summary_dt)))
setorder(summary_dt, test, category)

num_cols <- names(summary_dt)[sapply(summary_dt, is.numeric)]
print(copy(summary_dt)[, (num_cols) := lapply(.SD, signif, 3), .SDcols = num_cols][])
if (nrow(pairwise_dt) > 0) print(pairwise_dt)


### 5. PLOTS
plot_groups <- function(res){
  if (nrow(res$data) == 0) return(NULL)
  plot_dt <- melt(res$data, id.vars = c('context_id', 'group'),
                  measure.vars = covar_info$var, variable.name = 'var')
  plot_dt <- merge(plot_dt, covar_info, by = 'var')
  # log10 scale for the most skewed variables
  plot_dt[var %in% c('population', 'cases_per_10k'), value := log10(value + 1)]
  plot_dt[, label := factor(label, levels = covar_info$label)]
  # annotate each panel with the Kruskal-Wallis p-value
  pvals <- merge(res$summary[, .(var = variable, kruskal_p)], covar_info[, .(var, label)], by = 'var')
  pvals[, label := factor(label, levels = covar_info$label)]

  ggplot(plot_dt[!is.na(value)], aes(x = group, y = value)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    geom_text(data = pvals, aes(x = -Inf, y = Inf, label = paste0('KW p = ', signif(kruskal_p, 2))),
              hjust = -0.1, vjust = 1.5, size = 3) +
    facet_wrap(~label, scales = 'free_y') +
    scale_x_discrete(name = NULL, labels = function(x) substr(x, 1, 1)) +
    scale_y_continuous(name = NULL) +
    theme_bw() +
    ggtitle(paste0(res$test, ', ', res$category, ' closures: context characteristics by verdict group'),
            subtitle = paste(paste(group_labels, collapse = '; '), '\n(population and cases shown as log10(x + 1))'))
}

# one plot per test x mandate category
plots <- Filter(Negate(is.null), lapply(results, plot_groups))
for (p in plots) print(p)
