dispersal_dat=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/salamander_dispersal.csv")
dispersal_dat
    # Dispersal of juvenile marbled salamanders from their natal ponds to neighboring ponds
    # 14 vernal pools --> from 1999-2004
    # Juveniles marked and recaptured at non-natal ponds for dispersal rates
    # dist.class = distance class, based on 100 m intervals;
    # disp.rate.ftb = standardized dispersal rate for first-time breeders, which can be interpreted as a relative dispersal probability.
    # disp.rate.eb = standardized dispersal rate for experienced breeders, which can be interpreted as arelative dispersal probability.
    
dev.off()

require(here)
png(
  filename ="exp_plotQ1.png")
plot(dispersal_dat$dist.class, dispersal_dat$disp.rate.ftb)

exp_fun= function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  ylim= c(0,2),
    exp_fun(x, 1.9, 0.1), 
  from = 0, to = 15, add = FALSE, 
  main = "Exponential plots of 4 curves with\ndifferent parameter values ", 
  ylab = "Distance Rate", xlab = "Dispersal Class", col= "black", lty = "solid")
curve(
  ylim= c(0,2),
  exp_fun(x, 1.9, 0.3), 
  from = 0, to = 15, add = TRUE, 
  main = "Exponential plots of 4 curves with\ndifferent parameter values", 
   col = "black", lty = "dotted")
curve(
  ylim= c(0,2),
  exp_fun(x, 1.2, 0.2), 
  from = 0, to = 15, add = TRUE, 
  main = "Exponential plots of 4 curves with\ndifferent parameter values", 
   col = "red", lty = "solid")
curve(
  ylim= c(0,2),
  exp_fun(x, 1.2, 0.4), 
  from = 0, to = 15, add = TRUE, 
  main = "Exponential plots of 4 curves with\ndifferent parameter values", 
   col = "red", lty = "dotted")

dev.off()
