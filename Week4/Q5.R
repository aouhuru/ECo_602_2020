#CreateDataframe
require(here)
png(
  filename = here("sim_data_scatterplots.png"),
  width = 1000, height = 1000 , res = 180 , units = "px", bg = "transparent")

par(mfrow=c(2,2))
n_pts = 15
x_min = 0
x_max = 15
set.seed(44)
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

Sim2=plot(y_observed ~ x, data = dat, pch = 6, main= "Plot 1 of 15 randomly\ndistributed numbers", xlab= "Values", ylab = "Points")

n_pts = 13
x_min = 0
x_max = 15
set.seed(44)
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

Sim1=plot(y_observed ~ x, data = dat, pch = 17, main= "Plot 1 of 13 randomly\ndistributed numbers", xlab= "Values", ylab = "Points")
#3
n_pts = 15
x_min = 0
x_max = 15
set.seed(43)
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

Sim3=plot(y_observed ~ x, data = dat, pch = 6, main= "Plot 2 of 15 randomly\ndistributed numbers", xlab= "Values", ylab = "Points")

#4
n_pts = 13
x_min = 0
x_max = 15
set.seed(43)
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

Sim4=plot(y_observed ~ x, data = dat, pch = 17, main= "Plot 2 of 13 randomly\ndistributed numbers", xlab= "Values", ylab = "Points")

dev.off()
