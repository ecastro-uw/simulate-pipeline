
# obs data
# sim_dat
ggplot(sim_dat, aes(x=time_id,y=y)) +
  geom_point() +
  facet_wrap(~location_id)

# adjusted values
# results_output
all_data <- merge(sim_dat, results_output, by=c('location_id', 'time_id'), all.x=T)

ggplot(all_data[time_id>=-4]) +
  geom_point(aes(x=time_id, y=y), size=0.5) +
  geom_line(aes(x=time_id, y=mean)) +
  geom_errorbar(data=all_data[time_id==0], aes(x=0, ymin=q2.5,ymax=q97.5), color='red', width=0.4) +
  facet_wrap(~location_id, ncol=8) +
  theme_bw()

# try color coding the panel
library(ggh4x)

# Create strip color mapping
strip_colors <- all_data[time_id == 0, .(strip_color = ifelse(y < q2.5, "green4", "red")), by = location_id]
all_data <- merge(all_data, strip_colors, by = "location_id", all.x = TRUE)

# Get unique location_ids in order and their corresponding colors
unique_locs <- unique(all_data[time_id >= -4]$location_id)
strip_fill <- strip_colors$strip_color[match(unique_locs, strip_colors$location_id)]

ggplot(all_data[time_id >= -4]) +
  geom_point(aes(x = time_id, y = y), size = 0.5) +
  geom_line(aes(x = time_id, y = mean)) +
  geom_errorbar(data = all_data[time_id == 0], 
                aes(x = 0, ymin = q2.5, ymax = q97.5, 
                color=y < q2.5), width = 0.4) +
  scale_color_manual(values=c("TRUE"="green4", "FALSE"="red")) +
  facet_wrap2(~location_id, ncol = 8, 
              strip = strip_themed(background_x = elem_list_rect(fill = strip_fill))) +
  theme_bw() +
  guides(color="none")




ggplot(all_data[time_id==0]) +
  geom_errorbar(aes(x=location_id, ymin=q2.5,ymax=q97.5), color='red', width=0.7) +
  geom_point(aes(x=location_id, y=y), size=1) +
  theme_bw() 

ggplot(all_data[time_id==0]) +
  geom_errorbar(aes(x=location_id, ymin=q2.5, ymax=q97.5, 
                    color=y < q2.5), width=0.7) +
  scale_color_manual(values=c("TRUE"="green", "FALSE"="red")) +
  geom_point(aes(x=location_id, y=y), size=1) +
  theme_bw() +
  guides(color="none")


# draws of unadjusted values
unadj_draws <- unadj_results[location_id==7 & time_id==0, .SD, .SDcols=draw_cols]
unadj_long <- melt(unadj_draws, measure.vars=draw_cols, variable.name='draw', value='unadj')

# draws of adjusted values
adj_draws <- final_results[location_id==7 & time_id==0, .SD, .SDcols=draw_cols]
adj_long <- melt(plot_draws, measure.vars=draw_cols, variable.name='draw', value='adj')

# draws
draws <- merge(unadj_long, adj_long, by='draw')

# summary unadj
pre_summary <- pre_adj_output[location_id==7]

#summary adj
adj_summary <- results_output[location_id==7 & time_id==0]

# figures for slide deck
obs_dt <- sim_dat[location_id==7]

# observed time series (pre-event)
ggplot(obs_dt[time_id<0], aes(x=time_id,y=y)) +
  geom_point(size=2) +
  scale_x_continuous(limits=c(-8,0), name='Time Step') +
  scale_y_continuous(limits=c(-17,2), name='Outcome \n(log space)') +
  theme_bw() +
  theme(axis.text=element_text(size=12))

# unadj preds 
unadj_fill <- data.frame(
  x = c(-1, 0, 0),
  y = c(obs_dt[time_id==-1]$y, pre_summary$q2.5, pre_summary$q97.5)
)

ggplot() +
  geom_point(data=obs_dt[time_id<0], aes(x=time_id,y=y), size=2) +
  #geom_violin(data=draws, aes(x=0,y=unadj), fill="lightblue") +
  #geom_errorbar(data = pre_summary,
  #             aes(x=0, ymin=q2.5, ymax=q97.5), color='red', width=2) +
  geom_polygon(data = unadj_fill, aes(x=x, y=y), 
               fill = "red", alpha = 0.5) +
  scale_x_continuous(limits=c(-8,0), name='Time Step') +
  scale_y_continuous(limits=c(-17,2), name='Outcome \n(log space)') +
  theme_bw() +
  theme(axis.text=element_text(size=12))

# adj preds 
adj_fill <- data.frame(
  x = c(-1, 0, 0),
  y = c(obs_dt[time_id==-1]$y, adj_summary$q2.5, adj_summary$q97.5)
)
ggplot() +
  geom_point(data=obs_dt[time_id<0], aes(x=time_id,y=y), size=2) +
  #geom_violin(data=draws, aes(x=0,y=adj), fill="lightblue") +
  #geom_errorbar(data = adj_summary,
  #             aes(x=0, ymin=q2.5, ymax=q97.5), color='red', width=2) +
  geom_polygon(data = adj_fill, aes(x=x, y=y), 
               fill = "red", alpha = 0.5) +
  scale_x_continuous(limits=c(-8,0), name='Time Step') +
  scale_y_continuous(limits=c(-17,2), name='Outcome \n(log space)') +
  theme_bw() +
  theme(axis.text=element_text(size=12))

# add last obs value
ggplot() +
  geom_point(data=obs_dt, aes(x=time_id,y=y), size=2) +
  geom_polygon(data = adj_fill, aes(x=x, y=y), 
               fill = "red", alpha = 0.5) +
  scale_x_continuous(limits=c(-8,0), name='Time Step') +
  scale_y_continuous(limits=c(-17,2), name='Outcome \n(log space)') +
  theme_bw() +
  theme(axis.text=element_text(size=12))



ggplot(all_data[location_id==7 & time_id<0], aes(x=time_id,y=y)) +
  geom_ribbon(aes(ymin=q2.5,ymax=q97.5), fill="lightblue") +
  geom_point() +
  scale_x_continuous(limits=c(-8,0)) +
  theme_bw()


# make a beeswarm plot for one location
library(ggbeeswarm)
loc_id <- 28

one_loc <- final_results[location_id==loc_id & time_id==0]
one_loc_long <- melt(one_loc, id.vars = c('time_id','y'), measure.vars=draw_cols,
                     variable.name = "draw", value.name = "pred")

ggplot(one_loc_long, aes(x=time_id, y=pred)) +
  geom_beeswarm(alpha=1) +
  geom_point(data=sim_dat[location_id==loc_id & time_id==0], aes(x=time_id,y=y), 
             color='red', pch=17) +
  scale_x_continuous(name="Time Step", breaks=c(0)) +
  scale_y_continuous(name="Outcome \n(log space)") +
  theme_bw() 
