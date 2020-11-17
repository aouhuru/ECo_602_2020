ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue", xlim= c(0,1600), ylim = c(0,1))
curve(
  ricker_fun(x, 0.0082, 0.0045),add = TRUE)


#Question 11## 

resids_ricker= dat_dispersal$disp.rate.ftb -
  dat_dispersal$dist.class
  ricker_fun(x, 0.0082, 0.0045)

png(
  filename = "Q11.png"
)  
par(mfrow= c(2,1))
hist(resids_ricker)

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue", xlim= c(0,1600), ylim = c(0,1))
curve(
  ricker_fun(x, 0.0082, 0.0045),add = TRUE)
dev.off()