#Question1:Consider the first bird species you chose to examine.
#Q1 (1 pt). Upload an image file of the logistic function plot.
#Q2 (3 pts). What was the bird species? Describe its presence/absence patterns in terms of basal area. Did it seem to prefer areas with high or low tree cover?  
#Steller Jays seem to 
STJA
STJA_present_absent= as.numeric(dat_all$STJA >0)
STJA_present_absent

plot(x=dat_all$ba.tot, y= STJA_present_absent, cex= 0.6)
curve(logistic_midpoint_slope(x, midpoint = 25, slope =  1), add= TRUE)
#Question2: Consider the second bird species you chose to examine.
#Q1 (1 pt). Upload an image file of the logistic function plot.
#Q2 (3 pts). What was the bird species? Describe its presence/absence patterns in terms of basal area. Did it seem to prefer areas with high or low tree cover?
CBCH
CBCH_present_absent = as.numeric(dat_all$CBCH > 0)

plot(x= dat_all$ba.tot, y= CBCH_present_absent, cex=0.7)
curve(logistic_midpoint_slope(x, midpoint = 35, slope =  1), add= TRUE)


#Question 3:Using the dat_all data frame, calculate the total number of Gray Jays observed in all of the sampling sites.
#Q1 (1pt): Past the R code you used for the calculation into the text entry window.
GRJA_tot=sum(dat_all$GRJA)
GRJA_tot



#Question 4: Using the dat_all data frame, calculate the total number of sampling sites in which Gray Jays were observed.
#Hint: What happens when you use the sum() function on a vector of Boolean values?
#Q1 (1pt): Past the R code you used for the calculation into the text entry window.
Sites_GRJA= sum(dat_all$GRJA ==1)
Sites_GRJA




