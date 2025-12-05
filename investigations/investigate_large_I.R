library(ggplot2)
library(gridExtra)

version_id <- '20251201.04'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

## load configs
configs <- read_yaml(file.path(root,'config_sim.yaml'))
theta <- configs$theta

## find a rep with an inflation factor that is approaching 10 ##

# list all coverage files
coverage_dts <- list.files(dir, pattern="coverage_p", full.names = T)

# define function for processing each coverage file
load_file <- function(file){
  dt <- fread(file)
  dt[, param_id := as.numeric(str_extract(file, "(?<=_p)\\d+"))]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
} 

# load and compile the files
coverage_dt <- rbindlist(lapply(coverage_dts, load_file))

# grab first rep where inflation factor wants to be 10
rep <- coverage_dt[min(which(coverage_dt$multiplier > 9.99))]
p <- rep$param_id
b <- rep$batch_id
r <- rep$rep_id

# load pre-adjusted draws for all locations and time steps from the desired rep
pred_pre <- fread(paste0(dir,'/pred_pre_p',p,'_b',b,'.csv'))[rep_id==r]
draws <- log(pred_pre[, 4:1003])
pred_pre <- cbind(pred_pre[,1:3], draws)

# load the observed values
obs <- fread(paste0(dir,'/obs_p',p,'_b',b,'.csv'))[rep_id==r]
obs[, y:=log(y)]

# add theta back to final observation
#obs[time_id==0, y:=y+theta]

# reshape wide to long
pred_pre_long <- melt(pred_pre, id.vars = c('location_id', 'time_id'),
                      measure.vars = paste0('draw_',1:1000),
                      variable.name = 'draw',
                      value.name = 'pred')
pred_pre_long[,draw:=as.numeric(gsub('draw_','',draw))]

# add observations
pred_pre_long <- merge(pred_pre_long, obs[,.(location_id, time_id, obs=y)], by=c('location_id', 'time_id'), all.x=T)


# Define the values for l and t
l_values <- unique(pred_pre_long$location_id)
t_values <- unique(pred_pre_long$time_id)

# Create a list to store all plots
plot_list <- list()

# Loop through all combinations of l and t
for (l in l_values) {
  for (t in t_values) {
    small_dt <- pred_pre_long[location_id == l & time_id == t]
    
    pl <- ggplot(small_dt, aes(x = pred)) +
      geom_density(fill = "lightblue", alpha = 0.7) + 
      geom_vline(xintercept = unique(small_dt$obs), color = "red", 
                 linewidth = 0.5) +
      labs(title = paste("l =", l, ", t =", t),
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(size = 8)) +
      theme(axis.text.y = element_blank())
    
    plot_list[[length(plot_list) + 1]] <- pl
  }
}

# Arrange all plots in a grid (10 rows x 5 columns)
grid.arrange(grobs = plot_list, ncol = 5, nrow = 10)



## Remake the same plot but with post-adjusted values ##

# load pre-adjusted draws for all locations and time steps from the desired rep
pred_adj <- fread(paste0(dir,'/pred_adj_p',p,'_b',b,'.csv'))[rep_id==r]
draws <- log(pred_adj[, 4:1003])
pred_adj <- cbind(pred_adj[,1:3], draws)

# reshape wide to long
pred_adj_long <- melt(pred_adj, id.vars = c('location_id', 'time_id'),
                      measure.vars = paste0('draw_',1:1000),
                      variable.name = 'draw',
                      value.name = 'pred')
pred_adj_long[,draw:=as.numeric(gsub('draw_','',draw))]

# add observations
pred_adj_long <- merge(pred_adj_long, obs[,.(location_id, time_id, obs=y)], by=c('location_id', 'time_id'), all.x=T)

# Create a list to store all plots
plot_list_adj <- list()

# Loop through all combinations of l and t
for (l in l_values) {
  for (t in t_values) {
    small_dt <- pred_adj_long[location_id == l & time_id == t]
    
    pl <- ggplot(small_dt, aes(x = pred)) +
      geom_density(fill = "lightblue", alpha = 0.7) + 
      geom_vline(xintercept = unique(small_dt$obs), color = "red", 
                 linewidth = 0.5) +
      labs(title = paste("l =", l, ", t =", t),
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(size = 8)) +
      theme(axis.text.y = element_blank())
    
    plot_list_adj[[length(plot_list_adj) + 1]] <- pl
  }
}

# Arrange all plots in a grid (10 rows x 5 columns)
grid.arrange(grobs = plot_list_adj, ncol = 5, nrow = 10)


###
setnames(pred_adj_long, 'pred', 'pred_adj')
all <- merge(pred_adj_long[,.(location_id, time_id, draw, pred_adj)], pred_pre_long,
      by=c('location_id', 'time_id', 'draw'))


plot_list_all <- list()
# Loop through all combinations of l and t
for (l in l_values) {
  for (t in t_values) {
    small_dt <- all[location_id == l & time_id == t]
    
    pl <- ggplot(small_dt, aes(x = pred)) +
      geom_density(aes(x=pred), fill = "lightblue", alpha = 0.7) +
      geom_density(aes(x=pred_adj), fill = "blue", alpha = 0.7) +
      geom_vline(xintercept = unique(small_dt$obs), color = "red", 
                 linewidth = 0.5) +
      labs(title = paste("l =", l, ", t =", t),
           x = "",
           y = "") +
      theme_minimal() +
      theme(plot.title = element_text(size = 8)) +
      theme(axis.text.y = element_blank())
    
    plot_list_all[[length(plot_list_all) + 1]] <- pl
  }
}
grid.arrange(grobs = plot_list_all, ncol = 5, nrow = 10)

pdf(file.path(root,'investigation_v2.pdf'), width=14, height=10)
grid.arrange(grobs = plot_list, ncol = 5, nrow = 10)
grid.arrange(grobs = plot_list_adj, ncol = 5, nrow = 10)
plot_list_all[[1]]
plot_list_all[[2]]
plot_list_all[[3]]
plot_list_all[[4]]
plot_list_all[[5]]
dev.off()


pdf(file.path(root,'investigation_v3.pdf'), width=14, height=10)
grid.arrange(grobs = plot_list, ncol = 5, nrow = 10)
dev.off()


cov_by_I <- data.table()
for (i in seq(0,10,1)){
  temp <- data.table(I=i, coverage = adj_and_check(I=i, dt_summary))
  cov_by_I <- rbind(cov_by_I, temp)
}

pdf(file.path(root,'coverage_by_I.pdf'), width=7, height=5)
ggplot(cov_by_I, aes(x=I,y=coverage)) + 
  geom_point() + 
  geom_line()
dev.off()



## L=50 and sigma.s=0.2
pred_pre$log_mean <- rowMeans(pred_pre[, 4:1003])
pred_pre$log_ll <- apply(pred_pre[, 4:1003], 1, quantile, 0.025)
pred_pre$log_ul <- apply(pred_pre[, 4:1003], 1, quantile, 0.975)
small <- pred_pre[,.(location_id, time_id, log_mean, log_ll, log_ul)]
small <- merge(small, obs[,.(location_id, time_id, log_obs = y)],
               by=c('location_id', 'time_id'), all.x=T)
dt_summary <- small

cov_by_I <- data.table()
for (i in seq(0,10,1)){
  temp <- data.table(I=i, coverage = adj_and_check(I=i, dt_summary))
  cov_by_I <- rbind(cov_by_I, temp)
}

pdf(file.path(root,'coverage_by_I.pdf'), width=7, height=5)
ggplot(cov_by_I, aes(x=I,y=coverage)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0.95, linetype='dashed', color='red')

ggplot(cov_by_I, aes(x=I,y=coverage)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0.95, linetype='dashed', color='red') +
  annotate("point", x=9.9, y=0.999, color='blue', shape=17, size=2) +
  scale_y_continuous(limits = c(0.75,1)) 
dev.off()
