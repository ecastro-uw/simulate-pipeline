
dt <- fread('/mnt/share/scratch/users/ems2285/thesis/power_curve_data.csv')

ggplot(dt, aes(x=sigma.s,y=L, color=as.character(theta))) +
  geom_line()
