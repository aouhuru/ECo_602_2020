dat_hab=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
dat_bird=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
dat_bird
dat_hab
dat_bird_hab=merge(dat_bird,dat_hab)
dat_bird_hab
par(mfrow=c(1,3))
hist(dat_bird_hab$elev, xlab = "Elev", main = "Frequency of Elevation")
hist(dat_bird_hab$slope, xlab = "Slope", main= "Frequency of slope", freq = FALSE, xlim = c(0,120), ylim = c(0, 0.020))
hist(dat_bird_hab$aspect, xlab = "Aspect", main= "Frequency of Aspect")
