
# Load location hierarcy
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
state_map <- hierarchy[level==2, .(location_id, state_abbrev = gsub('US-','',local_id))]
states_dt <- merge(hierarchy[level==3, .(location_id, location_name, parent_id)],
                   state_map, 
                   by.x='parent_id', by.y='location_id')

# Load dropped location log
dropped_dt <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/dropped_locations_inv2_0628.csv')

# Plot reason for exclusion, by mandate type
ggplot(dropped_dt, aes(x=event, fill=reason)) +
  geom_bar(position="stack") +
  scale_x_discrete("Mandate Type") +
  scale_y_continuous(name="Number of counties dropped from analysis") +
  theme_classic()


# among 2nd restaurant mandates, reason for exclusion by state
dropped_dt <- merge(dropped_dt, states_dt[, .(location_id, state_abbrev)], by='location_id', all.x=T)
ggplot(dropped_dt[event=='second_restaurant'], aes(x=state_abbrev, fill=reason)) +
  geom_bar(position="stack") +
  scale_x_discrete("State") +
  scale_y_continuous("Number of counties dropped") +
  theme_classic()


# among 2nd bar mandates, reason for exclusion by state
ggplot(dropped_dt[event=='second_bar'], aes(x=state_abbrev, fill=reason)) +
  geom_bar(position="stack") +
  scale_x_discrete("State") +
  scale_y_continuous("Number of counties dropped") +
  theme_classic()



dt <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_0628.csv')
dt_pre <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_TWO_WEEKS_PRE_v2.csv')
dt_post <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_TWO_WEEKS_POST_v2.csv')

locs <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/locs_by_context_inv2_0628.csv')[context_id==106]
locs_pre <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/locs_by_context_inv2_TWO_WEEKS_PRE_v2.csv')[context_id==106]
locs_post <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/locs_by_context_inv2_TWO_WEEKS_POST_v2.csv')[context_id==106]


dropped <-  fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/dropped_locations_inv2_0628.csv')[event=='second_restaurant']
dropped_post <-  fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/dropped_locations_inv2_TWO_WEEKS_POST_v2.csv')[event=='second_restaurant']

wa_states <- hierarchy[parent_id==570, location_id]
dropped[location_id %in% wa_states]
dropped_post[location_id %in% wa_states]

wy_states <- hierarchy[parent_id==573, location_id]
dropped[location_id %in% wy_states]
dropped_post[location_id %in% wy_states]


dt <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_0628.csv')[, .(context_id, mandate_type, mandate_num, state_abbrev, t0=N)]
dt_2pre <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_TWO_WEEKS_PRE_v2.csv')[, .(mandate_type, mandate_num, state_abbrev, `t-2`=N)]
dt_1pre <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_ONE_WEEK_PRE_v2.csv')[, .(mandate_type, mandate_num, state_abbrev, `t-1`=N)]
dt_1post <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_ONE_WEEK_POST_v2.csv')[, .(mandate_type, mandate_num, state_abbrev, `t+1`=N)]
dt_2post <- fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/context_lookup_inv2_TWO_WEEKS_POST_v2.csv')[, .(mandate_type, mandate_num, state_abbrev, `t+2`=N)]


dt <- merge(dt, dt_2pre, by=c('mandate_type', 'mandate_num', 'state_abbrev'), all=T)
dt <- merge(dt, dt_1pre, by=c('mandate_type', 'mandate_num', 'state_abbrev'), all=T)
dt <- merge(dt, dt_1post, by=c('mandate_type', 'mandate_num', 'state_abbrev'), all=T)
dt <- merge(dt, dt_2post, by=c('mandate_type', 'mandate_num', 'state_abbrev'), all=T)



dropped <-  fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/dropped_locations_inv2_0628.csv')[event=='second_bar']
dropped_post <-  fread('/ihme/homes/ems2285/repos/simulate-pipeline/config_files/dropped_locations_inv2_TWO_WEEKS_POST_v2.csv')[event=='second_bar']

az_states <- hierarchy[parent_id==525, location_id]
dropped[location_id %in% az_states]
dropped_post[location_id %in% az_states]


