# plot M vs coverage

#x
mult_vec <- seq(0,2,by=0.02)

#y
coverage_vec <- c()
for (m in mult_vec){
  coverage <- calc_adj_coverage(m, results_draws, by_draw=T)$coverage_post
  coverage_vec <- c(coverage_vec, coverage)
}

plot_dt <- data.table(M=mult_vec, coverage=coverage_vec)

p1 <- ggplot(plot_dt, aes(x=M, y=coverage)) +
  geom_line() +
  annotate(geom='point', x=1,y=plot_dt[M==1,coverage]) +
  annotate(geom='text', x=1,y=plot_dt[M==1,coverage]-0.05, label=paste0('(1,',plot_dt[M==1,coverage],')')) +
  annotate(geom='point', y=0.95,x=plot_dt[coverage==0.95,M]) +
  annotate(geom='text', y=0.9,x=plot_dt[coverage==0.95,M], label=paste0('(',plot_dt[coverage==0.95,M],',0.95)')) +
  geom_hline(yintercept = 0.95, linetype='dashed') +
  theme_bw()



# plot M vs val
val_vec <- c()
for (r in 1:nrow(plot_dt)){
  coverage <- plot_dt[r,coverage]
  M <- plot_dt[r,M]
  val <- ((coverage - target_cov)^2)+((M-1)^2/100000)
  val_vec <- c(val_vec, val)
}

plot_dt$val <- val_vec

p2 <- ggplot(plot_dt, aes(x=M, y=val)) +
  geom_line() +
  annotate(geom='point',x=1,y=plot_dt[M==1,val]) +
  annotate(geom='text', x=1, y=plot_dt[M==1,val]+0.07,
           label=paste0('(1,',round(plot_dt[M==1,val],4),')')) +
  annotate(geom='point',y=min(plot_dt$val), x=plot_dt[val==min(val),M]) +
  annotate(geom='text', y=min(plot_dt$val)+0.07, x=plot_dt[val==min(val),M]+.18,
           label=paste0('(',plot_dt[val==min(val),M],',',min(plot_dt$val),')')) +
  theme_bw()

p3 <- ggplot(plot_dt[M>0.9 & M<1.5], aes(x=M, y=val)) +
  geom_line() +
  annotate(geom='point',y=min(plot_dt$val), x=plot_dt[val==min(val),M]) +
  theme_bw() 

pdf(file.path(out_dir, 'optim_diagnostics.pdf'), width=7, height=5)
p1
p2
p3
dev.off()
