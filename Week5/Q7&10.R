##QUESTION &##
png(
  filename = "Exp_Q7.png"
)
exp_fun= function(x, a, b) 
{
  return(a * exp(-b * x))
}
plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")
curve(
  ylim= c(0,1600),xlim = c(0,0.9),
  exp_fun(x, 1.1, 0.002), 
  from = 0, to = 1600, add = TRUE , 
  main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")
dev.off()
####QUESTION 10###
resids_exp= dat_dispersal$disp.rate.ftb -
  dat_dispersal$dist.class 
  exp_fun(x, 1.1, 0.002)

png(
  filename = "Q10.png"
)  
par(mfrow= c(2,1))
  
hist(resids_exp)

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")
curve(
  ylim= c(0,1600),xlim = c(0,0.9),
  exp_fun(x, 1.1, 0.002), 
  from = 0, to = 1600, add = TRUE , 
  main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")
dev.off()