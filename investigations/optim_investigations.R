
forecasts_subset <- rbind(forecasts_subset, unadj_results2, fill=T)

# summarize
draw_cols <- paste0('draw_', 1:configs$d)
summary_dt <- as.data.table(t(apply(forecasts_subset[, .SD, .SDcols = draw_cols], 1, quantile, c(0.025, 0.975))))
summary_dt <- cbind(forecasts_subset[, .(model, location_id, time_id)], summary_dt)
summary_dt <- merge(summary_dt, obs_dt[! is.na(y), .(location_id, time_id, y)], by=c('location_id', 'time_id'), all.y=T)

loc_list <- unique(forecasts_subset$location_id)

# define jitter 
summary_dt[, model := factor(model, levels = unique(summary_dt$model))]
offsets <- seq(-0.3, 0.3, length.out = 3)
offset_map <- setNames(offsets, levels(summary_dt$model))
summary_dt[, x_jitter := time_id + offset_map[as.character(model)]]

i <- 1
ggplot(data=summary_dt[location_id == loc_list[i]]) +
  geom_errorbar(aes(x=x_jitter, ymin = `2.5%`, ymax = `97.5%`, color = model), width = 0.3, linewidth=1.1) +
  geom_point(aes(x=time_id, y=y), size=4, pch=18) +
  scale_x_continuous() +
  ggtitle(paste('Location ID', loc_list[i])) +
  theme_classic()






# plot the function to be minimized 
inputs <- seq(-9, 9, 0.25)

optim_dt <- data.table(
  input  = inputs,
  output = sapply(inputs, calc_wis, 
                  forecasts = forecasts_subset,
                  observations = obs_dt,
                  list_of_models = list_of_models,
                  d = d)
)

ggplot(optim_dt, aes(x=input, y=output)) + 
  geom_line() +
  annotate("point", x=fit$minimum,y=fit$objective, color="red", size=2) +
  scale_x_continuous(name="logit(weight)") +
  scale_y_continuous(name="WIS") + 
  theme_classic()




mod5_summary <- draws_dt[, `:=` (lower = apply(.SD, 1, quantile, 0.025), upper = apply(.SD, 1, quantile, 0.975)), .SDcols = paste0('draw_',1:1000)]
mod5_summary <- cbind(ids, mod5_summary[, .(lower, upper)])

mod14_summary <- draws_dt[, `:=` (lower = apply(.SD, 1, quantile, 0.025), upper = apply(.SD, 1, quantile, 0.975)), .SDcols = paste0('draw_',1:1000)]
mod14_summary <- cbind(ids, mod14_summary[, .(lower, upper)])

mod23_summary <- draws_dt[, `:=` (lower = apply(.SD, 1, quantile, 0.025), upper = apply(.SD, 1, quantile, 0.975)), .SDcols = paste0('draw_',1:1000)]
mod23_summary <- cbind(ids, mod23_summary[, .(lower, upper)])

p1 <- ggplot(data) +
  geom_point(aes(x=time_id,y=y)) +
  geom_errorbar(data=mod5_summary, aes(x=time_id, ymin=lower, ymax=upper)) +
  scale_y_continuous(limits=c(-3,1.5)) +
  facet_wrap(~location_id) +
  ggtitle('Model 5 (OLS)') +
  theme_classic() 

p2 <- ggplot(data) +
  geom_point(aes(x=time_id,y=y)) +
  geom_errorbar(data=mod14_summary, aes(x=time_id, ymin=lower, ymax=upper)) +
  scale_y_continuous(limits=c(-3,1.5)) +
  facet_wrap(~location_id) +
  ggtitle('Model 14 (GLS)') +
  theme_classic()

p3 <- ggplot(data) +
  geom_point(aes(x=time_id,y=y)) +
  geom_errorbar(data=mod23_summary, aes(x=time_id, ymin=lower, ymax=upper)) +
  scale_y_continuous(limits=c(-3,1.5)) +
  facet_wrap(~location_id) +
  ggtitle('Model 23 (LME)') +
  theme_classic()

pdf('/ihme/scratch/users/ems2285/thesis/aim_3/select_candidate_models_context_10.pdf')
p1
p2
p3
dev.off()
