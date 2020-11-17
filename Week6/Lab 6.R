#introduction
#FUNCTION FOR THE LAB
sse_mean= function(x) {return (sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))}

require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)


#The Penguin Data
boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)


dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

#RESAMPLING WITH REPLACEMENT
# for reproduceability
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")


#CLASSICAL T_TEST 
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)


#TWO_SAMPLE RESAMPLE
# for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

#CLASSICAL TEST ON RESAMPLED DATA
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#DIFFERENCE OF MEANS
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

#DIFFERENCE IN MEANS IS
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)


#USING AGGREGATED
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = mean, 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed

#SAMPLE SIZES
table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

#SIMULATION FUNCTION
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)


diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

#TWO_GROUP_RESAMPLE 
x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

two_group_resample = function(x, n_1, n_2) 
{
  x = dat_pen$flipper_length_mm
  n_1 = 68
  n_2 = 152
  
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  diff_simulated
}
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

#Resampling experiment
n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

#Retrieving named Elements
t_test = t.test(flipper_shuffled ~ dat_pen$species)

#examine object
str(t_test)

#Estimate object
t_test$estimate




##QUESTION 1 #### 
  #FUNCTION FOR SSE 
sse_mean= function(x) {return (sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)


### QUESTION 2 ###
  # x: A numeric vector
  # n_1: The number of values (with replacement) to take from x for the first group.
  # n_2: The number of values (with replacement) to take from x for the second group.
  # Your function should return:
    #A single numeric value: the difference in means between the two groups
two_group_resample = function(x, n_1, n_2) 
{
  x = dat_pen$flipper_length_mm
  n_1 = 80
  n_2 = 162
  
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)
}
set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 80, 162)

#return good for practice --)
### Question 3 - Resampling penguin data: histogram

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 80, 162)
  )
}
hist(mean_differences)

### Question 4 - Resampling penguin data - significance

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
#  ~ as explained by/ in terms of  
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

#Expected vs observed
sum(abs(mean_differences) > diff_observed)
sum(abs(mean_differences) < diff_observed)


## Question 5 ##
#give a p value less than 1 per 10 million
# how many simulations would you need for length to equal or greater than 5.8
#p value in terms of the observed

  #they are directly different, it you have a small p value tyou will need a very large length in order to see a length equal or greater than 5.8 


## Question 6 ##. --> Resampling

t.test(dat_pen$bill_depth_mm ~ dat_pen$species)
bill_dat = sample(penguins$bill_depth_mm, replace =  TRUE)
## Boxplot
boxplot(bill_dat ~ species, data = penguins, xlab = "species", ylab = "Bill Depth", main = "Bill Depths of 3 penguin species\nAdelie, Chinstrap, Gentoo")

#group means and differences between the means
diff_crit = aggregate(
  penguins$bill_depth_mm ~ dat_pen$species, 
  data = dat_pen, FUN = mean, na.rm = TRUE
)
diff_observed = diff(diff_crit[,2])
name_unknown
diff_observed
##Question 7 ### Resampling

## Question 8### Resampling