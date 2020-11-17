#QUESTION ON THIS

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
png(
  filename = "ricker_Q8.png"
)
plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue", xlim= c(0,1600), ylim = c(0,1))
curve(
  ricker_fun(x, 0.0082, 0.0045),add = TRUE)
dev.off()

####Question 9#####

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, 
     main = "Dispersal rate of first time breeding\nsalamanders and the distance class", 
     xlab = "Distance Class (100 m)", 
     ylab = "First Time Breeders",col = "dark blue", 
     xlim= c(0,1600), ylim = c(0,1))


guess_x = 920
guess_y = 0.30
guess_slope = -0.00055





#RESIDS
resids_linear= dat_dispersal$disp.rate.ftb -
  line_point_slope(x=dispersal_dat$dist.class, guess_x, guess_y, guess_slope)
png(
  filename = "Q9.png"
)
par(mfrow= c(2,1))
hist(resids_linear)

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue", xlim = c(-1,1500), ylim = c(0,1))
curve(
  line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)


dev.off()

