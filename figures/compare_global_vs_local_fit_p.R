dir <- "/ihme/scratch/users/ems2285/thesis/outputs/simulations/20251219.03"
param_id <- 'p1'
batch_id <- 'b1'
rep_id <- 1

configs <- read_yaml(paste0(dir,'/config_sim.yaml'))
obs_dt <- fread(paste0(dir,'/batched_output/obs_',param_id,'_',batch_id,'.csv'))[rep_id==1]
mods_dt <- fread(paste0(dir,'/batched_output/candidate_mods_',param_id,'_',batch_id,'.csv'))[rep_id==1]
ens_pre_dt <- fread(paste0(dir,'/batched_output/pred_pre_',param_id,'_',batch_id,'.csv'))[rep_id==1]
weights_dt <- fread(paste0(dir,'/batched_output/ens_weights_',param_id,'_',batch_id,'.csv'))[rep_id==1]
sigmas_dt <- fread(paste0(dir,'/batched_output/sigmas_',param_id,'_',batch_id,'.csv'))[rep_id==1]

# add theta back
obs_dt[time_id==0, y:=y+configs$theta]

# summarize candidate models
draw_cols <- paste0('draw_',1:configs$d)
mod_summary <- mods_dt[, mean := rowMeans(.SD), .SDcols = draw_cols][,.(model, location_id, time_id, mean)]

# summarize ensemble results (globally fit p)
ens_summary <- ens_pre_dt[, mean := rowMeans(.SD), .SDcols = draw_cols][,.(model = 'ensemble', location_id, time_id, mean)]

# ensemble with known p
p.s <- configs$p.s
p.f <- 1-p.s
draws <- mods_dt[model=='naive_flat_1', .SD, .SDcols = draw_cols] * p.f + mods_dt[model=='naive_slope_2', .SD, .SDcols = draw_cols] * p.s
draws[, mean := rowMeans(.SD), .SDcols = draw_cols]
ens_summary_2 <- cbind(ens_summary[,.(location_id, time_id)], draws[,.(mean)])
ens_summary_2$model <- 'Ensemble (known p)'

# combine
plot_dt <- rbind(ens_summary, mod_summary)
plot_dt <- rbind(plot_dt, ens_summary_2)

# plot
sigmas_dt$rep_id <- NULL
tbl_grob <- tableGrob(sigmas_dt, rows = NULL)
max_y <- max(plot_dt$mean)

pdf(paste0(dir,'/ensemble_weights.pdf'), width=7, height=5)
for(loc in unique(plot_dt$location_id)){
p <- ggplot() +
  geom_point(data=obs_dt[location_id==loc], aes(x=time_id, y=y)) +
  geom_line(data=plot_dt[location_id==loc], aes(x=time_id, y=mean, color=model)) +
  ggtitle(paste('sigma_f =', configs$sigma.f, 'sigma_s =', configs$sigma.s, 'p =', configs$p.s)) +
  scale_colour_manual(values = c('red', 'purple', 'blue', 'darkgreen'),
                      labels = c('Ensemble ',
                                 'Ensemble (known p)',
                                  paste0('Naive Flat (',round(weights_dt[model=='naive_flat_1',weight],2),')'),
                                  paste0('Naive Slope (',round(weights_dt[model=='naive_slope_2',weight],2),')')
                                   )) +
  theme_classic() 

#p + annotation_custom(
#  tbl_grob,
#  xmin = -8,  
#  xmax = -6,
#  ymin = max_y * 0.65,
#  ymax = max_y * 0.9
#)
print(p)
}
dev.off()






# ensemble with known p_s (0.5) 
ens_dt <- data.table(time_id=-4:0,
                     ens_p=log(predictions))
# recalculate predictions with fitted p_s (0.88)
ens_dt[, ens_p_hat:=log(predictions)]

plot_2 <- merge(sample_dt, ens_dt, by='time_id', all.x=T)

ggplot(plot_2) +
  geom_point(aes(x=time_id, y=obs)) +
  geom_line(aes(x=time_id, y=ens_p), color='green') +
  geom_line(aes(x=time_id, y=ens_p_hat), color='purple') +
  scale_y_continuous(name='y') +
  theme_classic()


# coverage with fitted values of sigmas
pred_int_dt <- data.table()
for(loc in 1:param_set$L){
  obs <- results[[loc]]$obs[, obs := log(y)]
  obs[, location_id := loc]
  draws <- log(results[[loc]]$ensemble[,-1])
  temp <- data.table(location_id = loc,
                     time_id = results[[loc]]$ensemble$time_id,
                     val = rowMeans(draws),
                     lower = apply(draws,1,quantile,0.025),
                     upper = apply(draws,1,quantile,0.975))
  temp <- merge(obs[,.(location_id, time_id,obs)], temp, by=c('location_id','time_id'), all.x=T)
  pred_int_dt <- rbind(pred_int_dt,temp)
}

pdf('/mnt/share/scratch/users/ems2285/thesis/outputs/simulations/20251205.01/coverage_fitted_sigmas.pdf', width=10, height=10)
for(loc in 1:param_set$L){
  p1<- ggplot(pred_int_dt[location_id==loc]) +
    geom_point(aes(x=time_id, y=obs)) +
    geom_point(aes(x=time_id, y=val), color='red') +
    geom_errorbar(aes(x=time_id, y=val, ymin=lower, ymax=upper), color='red') +
    #facet_wrap(~location_id, ncol=5) +
    ggtitle('Unadjusted coverage when using fitted sigmas')
  print(p1)
}
dev.off()

# coverage with known values of sigmas (0.1)
draws2 <- log(forecasts$ensemble[,-1])
temp2 <- data.table(time_id = forecasts$ensemble$time_id,
                    val = rowMeans(draws2),
                    lower = apply(draws2,1,quantile,0.025),
                    upper = apply(draws2,1,quantile,0.975))
plot_4 <- merge(obs_dt, temp2, by='time_id', all.x=T)
ggplot(plot_4) +
  geom_point(aes(x=time_id, y=obs)) +
  geom_point(aes(x=time_id, y=val), color='red') +
  geom_errorbar(aes(x=time_id, y=val, ymin=lower, ymax=upper), color='red') +
  ggtitle('Unadjusted coverage when using known sigmas')
