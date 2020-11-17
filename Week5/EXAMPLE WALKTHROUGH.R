#Rickker Function
# f(x)= a * x * e^-b*x
#b is the peak
#a -> determines the initial slope
#a and b together determine the height

#Creating a Ricker Function: 
ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}

#from, to --> range of x- values 
#add=FALSE --> creates new plot
curve(
  ricker_fun(x, 1, 1),
  from= 0, to = 5, add = FALSE,
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun= function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
exp_fun(x, 0.1, 0.5),
  from= 0, to = 10, add = FALSE,
  main = "exponential function",
  ylab = "f(x)", xlab = "x")
##### I HAVE QUESTION ABOUT HOW TO MAKE THIS LOOK LIKE THIS

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of poitns:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)

error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed)


error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
plot(x_sim, y_observed)

plot(x_sim, y_observed_2)
dev.off()

#Trying to use rexp() ***HOW TO MAKE IT WORK? 
y_observed_3=
  y_pred +
  rexp(n = n_pts, rate = 1.2)
plot(x_sim, y_observed_3) 

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")



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
  ylab = "??", xlab = "??", col= "black", lty = "solid")
curve(
  ylim= c(0,2),
  exp_fun(x, 1.2, 0.1), 
  from = 0, to = 15, add = TRUE, 
  main = "Exponential plots of 4 curves with\ndifferent parameter values", 
  ylab = "??", xlab = "??", col = "black", lty = "dotted")
