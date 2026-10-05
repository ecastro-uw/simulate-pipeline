# Combine context/location lookup files. Only keep the contexts/locations present across all 5 files.

# Root directory
root <- '/ihme/homes/ems2285/repos/simulate-pipeline/config_files/'

# load list of locations by context
locs <-       fread(paste0(root,'locs_by_context_inv2_0628.csv'))
locs_pre2 <-  fread(paste0(root,'locs_by_context_inv2_TWO_WEEKS_PRE_v2.csv'))
locs_pre1 <-  fread(paste0(root,'locs_by_context_inv2_ONE_WEEK_PRE_v2.csv'))
locs_post1 <- fread(paste0(root,'locs_by_context_inv2_ONE_WEEK_POST_v2.csv'))
locs_post2 <- fread(paste0(root,'locs_by_context_inv2_TWO_WEEKS_POST_v2.csv'))

# load mapping of context id to definition
dt <-       fread(paste0(root,'context_lookup_inv2_0628.csv'))[, .(context_id, mandate_type, mandate_num, state_abbrev)]
dt_pre2 <-  fread(paste0(root,'context_lookup_inv2_TWO_WEEKS_PRE_v2.csv'))[, .(context_id, mandate_type, mandate_num, state_abbrev)]
dt_pre1 <-  fread(paste0(root,'context_lookup_inv2_ONE_WEEK_PRE_v2.csv'))[, .(context_id, mandate_type, mandate_num, state_abbrev)]
dt_post1 <- fread(paste0(root,'context_lookup_inv2_ONE_WEEK_POST_v2.csv'))[, .(context_id, mandate_type, mandate_num, state_abbrev)]
dt_post2 <- fread(paste0(root,'context_lookup_inv2_TWO_WEEKS_POST_v2.csv'))[, .(context_id, mandate_type, mandate_num, state_abbrev)]

# add context definitions to loc lookup files
locs <-       merge(locs,       dt,       by='context_id')
locs_pre2 <-  merge(locs_pre2,  dt_pre2,  by='context_id')
locs_pre1 <-  merge(locs_pre1,  dt_pre1,  by='context_id')
locs_post1 <- merge(locs_post1, dt_post1, by='context_id')
locs_post2 <- merge(locs_post2, dt_post2, by='context_id')

# rename ids to differentiate
setnames(locs_pre2,  'context_id', 'context_id_pre2')
setnames(locs_pre1,  'context_id', 'context_id_pre1')
setnames(locs_post1, 'context_id', 'context_id_post1')
setnames(locs_post2, 'context_id', 'context_id_post2')

# combine, keeping only contexts & locations found in all 5 files
locs_intersect <- merge(locs, locs_pre2, by=c('mandate_type', 'mandate_num', 'state_abbrev', 'location_id'))
locs_intersect <- merge(locs_intersect, locs_pre1, by=c('mandate_type', 'mandate_num', 'state_abbrev', 'location_id'))
locs_intersect <- merge(locs_intersect, locs_post1, by=c('mandate_type', 'mandate_num', 'state_abbrev', 'location_id'))
locs_intersect <- merge(locs_intersect, locs_post2, by=c('mandate_type', 'mandate_num', 'state_abbrev', 'location_id'))

# remove any contexts with just one county
contexts_to_drop <- locs_intersect[, .N, by=context_id][N==1]$context_id
locs_intersect <- locs_intersect[! context_id %in% contexts_to_drop]

# assign new context ids
context_lookup_new <- unique(locs_intersect[, .(mandate_type, mandate_num, state_abbrev, old_context_id = context_id)])
context_lookup_new <- context_lookup_new[order(old_context_id)]
context_lookup_new[, context_id := .I]
context_lookup_new <- context_lookup_new[, .(context_id, mandate_type, mandate_num, state_abbrev)]

# create new loc lookup file
loc_lookup_new <- merge(context_lookup_new, locs_intersect[, .(mandate_type, mandate_num, state_abbrev, location_id)],
                        by=c('mandate_type', 'mandate_num', 'state_abbrev'))
loc_lookup_new <- loc_lookup_new[order(context_id)][, .(context_id, location_id)]
fwrite(loc_lookup_new, paste0(root, 'locs_by_context_inv2_intersect.csv'))

# Finalize the context lookup
context_lookup_new[, `:=` (country='USA',
                           ADMN=2,
                           outcome=ifelse(mandate_type=='restaurant', 'visits to restaurants', 'visits to bars'))]
context_lookup_new <- merge(context_lookup_new,
                            loc_lookup_new[, .N, by=context_id],
                            by='context_id')

# resolve models
key_cols <- c("mandate_type", "mandate_num", "state_abbrev")
model_cols <- paste0("model_", 1:11)
dt <-       fread(paste0(root,'context_lookup_inv2_0628.csv'))[, .SD, .SDcols = c(key_cols, model_cols)]
dt_pre2 <-  fread(paste0(root,'context_lookup_inv2_TWO_WEEKS_PRE_v2.csv'))[, .SD, .SDcols = c(key_cols, model_cols)]
dt_pre1 <-  fread(paste0(root,'context_lookup_inv2_ONE_WEEK_PRE_v2.csv'))[, .SD, .SDcols = c(key_cols, model_cols)]
dt_post1 <- fread(paste0(root,'context_lookup_inv2_ONE_WEEK_POST_v2.csv'))[, .SD, .SDcols = c(key_cols, model_cols)]
dt_post2 <- fread(paste0(root,'context_lookup_inv2_TWO_WEEKS_POST_v2.csv'))[, .SD, .SDcols = c(key_cols, model_cols)]

tables_list <- list(dt, dt_pre2, dt_pre1, dt_post1, dt_post2)
stacked <- rbindlist(tables_list)
stacked[, n_tables := .N, by = key_cols]
stacked <- stacked[n_tables == length(tables_list)] #only keep contexts that appear in all 5 tables
stacked[, n_tables := NULL]

result <- stacked[, lapply(.SD, min), by = key_cols, .SDcols = model_cols]

# add model vars to the context definition file
context_lookup_new <- merge(context_lookup_new, result, by=key_cols, all.x=T, sort=F)

# organize columns to match original file
col_names <- names(fread(paste0(root,'context_lookup_inv2_0628.csv')))
context_lookup_new <- context_lookup_new[, .SD, .SDcols = col_names]

fwrite(context_lookup_new, paste0(root, 'context_lookup_inv2_intersect.csv'))
