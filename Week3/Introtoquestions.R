hab=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv", TRUE)
birds=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv", TRUE)
  str(birds)
  str(hab)
  dat_all<-merge(hab,birds)
  sample(dat_all$CEWA, 100)
  
  
  plot(ba.tot ~ elev, data = dat_all)
  #checks to make sure information is corect
  sample(dat_bird$CEWA, 100)
  sample(dat_all[CEWA], 100)
  sample(dat_all$CEWA, 100)
  sample(dat_bird[CEWA], 100)
  #this will not run properly, CEWA is in the metadata for dat_bird, i have tried it both this way and for the merged information 
  

  #more walk through
  my_vec = rep(1:3, 5)
  my_vec == 3
  my_vec > 1
  as.numeric(my_vec > 1)
 
  cewa_present_absent= as.numeric(dat_all$CEWA >=1)
  cewa_present_absent
  
  install.packages("psych")
  require(psych)
  #I id not not get rhe boolean example,which is why this will not run 
  plot(x= dat_all$elev, y = cewa_present_absent)
  plot(x= dat_all$slope, y= cewa_present_absent)
  plot(x= dat_all$aspect, y= cewa_present_absent)
  
  # Function to calculate the logistic parameter a given the slope and midpoint
  get_logistic_param_a = function(slope, midpoint)
  {
    b = slope / 4
    return (-midpoint * (slope / 4))
  }
  
  # Function to calculate the logistic parameter b given the slope
  get_logistic_param_b = function(slope)
  {
    return (slope / 4)
  }
  
  
  # Calculate the value of the logistic function at x, given the parameters a and b.
  logistic = function(x, a, b)
  {
    val = exp(a + b * x)
    return(val / (1 + val))
  }
  
  # Calculate the value of the logistic function at x, given a slopoe and midpoint.
  logistic_midpoint_slope = function(x, midpoint, slope)
  {
    b = get_logistic_param_b(slope)
    a = get_logistic_param_a(slope, midpoint)
    return(logistic(x, a, b))
  }
  
  plot(x= dat_all$elev, y = cewa_present_absent, cex= 0.01)
  curve(logistic_midpoint_slope(x, midpoint = 400, slope =  0.1), add = TRUE)
#Negative
  plot(x = dat_all$elev, y = cewa_present_absent)
  curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
  
#shallow
  plot(x = dat_all$elev, y = cewa_present_absent)
  curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)
  #attempt at lab - No success, file too big. 
  pairs.panels(dat_bird)
  
  pairs.panels(dat_all$elev, 100) 

  
#Question1:Consider the first bird species you chose to examine.
#Q1 (1 pt). Upload an image file of the logistic function plot.
#Q2 (3 pts). What was the bird species? Describe its presence/absence patterns in terms of basal area. Did it seem to prefer areas with high or low tree cover?  

#Question2: Consider the second bird species you chose to examine.
#Q1 (1 pt). Upload an image file of the logistic function plot.
#Q2 (3 pts). What was the bird species? Describe its presence/absence patterns in terms of basal area. Did it seem to prefer areas with high or low tree cover?
    
#Question 3:Using the dat_all data frame, calculate the total number of Gray Jays observed in all of the sampling sites.
#Q1 (1pt): Past the R code you used for the calculation into the text entry window.
  
#Question 4: Using the dat_all data frame, calculate the total number of sampling sites in which Gray Jays were observed.
#Hint: What happens when you use the sum() function on a vector of Boolean values?
#Q1 (1pt): Past the R code you used for the calculation into the text entry window.

  