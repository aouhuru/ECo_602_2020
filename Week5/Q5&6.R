dat_dispersal=data.frame(dispersal_dat)
dat_dispersal
#5
png(filename = "scatterplot_Q5.png")
plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")
dev.off()
#Q2 -- how far salamanders moved
#Q3 -- how many salamanders moved

#6 Linear model
png(filename = "scatter_Linear_Q6.png")
plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue")

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





guess_x = 920
guess_y = 0.30
guess_slope = -0.00055

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb, main = "Dispersal rate of first time breeding\nsalamanders and the distance class", xlab = "Distance Class (100 m)", ylab = "First Time Breeders",col = "dark blue", xlim = c(-1,1500), ylim = c(0,1))
curve(
  line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)


dev.off()
             
